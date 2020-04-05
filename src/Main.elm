module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as D
import Json.Encode as JE
import Ports
import Process
import Task exposing (..)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { elementOfList : Maybe Dom.Viewport
    , elementOfListItem : Maybe Dom.Element
    , listScrollY : Float
    , listHeight : Float
    , pageX : Float
    , pageY : Float
    , startCursorX : Float
    , startCursorY : Float
    , selCenterY : Float
    , dir : Float
    , items : List Item
    , dragState : DragState
    }


type alias Item =
    { id : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , selected : Bool
    , isRange : Bool
    , content : String
    }


type DragState
    = Static
    | Fitting Int Item
    | Moving Int Item


init : () -> ( Model, Cmd Msg )
init _ =
    ( { elementOfList = Nothing
      , elementOfListItem = Nothing
      , listScrollY = 0
      , listHeight = 0
      , pageX = 0
      , pageY = 0
      , startCursorX = 0
      , startCursorY = 0
      , selCenterY = 0
      , dir = 0
      , items = []
      , dragState = Static
      }
    , Cmd.none
    )


getElementOfList : Cmd Msg
getElementOfList =
    Cmd.batch
        [ Task.attempt GetElementOfList (Dom.getViewportOf "scroll-list1")
        , Task.attempt GetElementOfListItem (Dom.getElement "scroll-list1-0")
        ]


ta : Float -> Item -> Task Dom.Error Item
ta scrollY item =
    Dom.getElement item.id
        |> Task.andThen
            (\e ->
                Task.succeed
                    { item
                        | x = e.element.x
                        , y = e.element.y + scrollY
                        , width = e.element.width
                        , height = e.element.height
                    }
            )


ti : Float -> List Item -> Task Dom.Error (List Item)
ti scrollY items =
    items |> List.map (ta scrollY) |> Task.sequence



-- UPDATE


type Msg
    = GetElementOfList (Result Dom.Error Dom.Viewport)
    | GetElementOfListItem (Result Dom.Error Dom.Element)
    | GetElements (Result Dom.Error (List Item))
    | ClickGetElement
    | ClickScroll
    | NoOp
    | DragMove Bool Float Float
    | EnteredDownBox
    | LeavedDownBox
    | DragStart Int Item Float Float
    | DragStop Float Float
    | InitList
    | ScrollList
    | GetScrollPosition (Result Dom.Error Dom.Viewport)
    | LoadListItem Int
    | ToStatic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetElementOfList (Ok element) ->
            ( { model | listScrollY = element.viewport.y, listHeight = element.viewport.height, elementOfList = Just element }, Cmd.none )

        GetElementOfList (Err error) ->
            ( model, Cmd.none )

        GetElementOfListItem (Ok element) ->
            ( { model | elementOfListItem = Just element }, Cmd.none )

        GetElementOfListItem (Err error) ->
            ( model, Cmd.none )

        ClickGetElement ->
            ( model
            , Cmd.batch
                [ Task.attempt GetElements (ti model.listScrollY model.items)
                , getElementOfList
                ]
            )

        ClickScroll ->
            ( model
            , scrollTo "scroll-list1" 500
            )

        NoOp ->
            ( model, Cmd.none )

        DragMove isLeftDown x y ->
            case isLeftDown of
                False ->
                    ( { model | dragState = Static, pageX = x, pageY = y }, Cmd.none )

                True ->
                    case model.dragState of
                        Moving index item ->
                            ( updateByDragMove index x y model, Cmd.none )

                        _ ->
                            ( { model | pageX = x, pageY = y }, Cmd.none )

        EnteredDownBox ->
            ( model, Cmd.none )

        LeavedDownBox ->
            ( model, Cmd.none )

        DragStart index item pageX pageY ->
            ( { model
                | dragState = Moving index item
                , pageX = pageX
                , pageY = pageY
                , startCursorX = pageX
                , startCursorY = pageY
                , selCenterY = (item.y - model.listScrollY) + item.height / 2
              }
            , Cmd.none
            )

        DragStop x y ->
            case model.dragState of
                Moving index item ->
                    ( updateByDragStop model, Task.perform (always ToStatic) (Process.sleep 400) )

                _ ->
                    ( { model | dragState = Static }, Cmd.none )

        ToStatic ->
            ( { model | dragState = Static }, Cmd.none )

        InitList ->
            let
                initItem c n =
                    { id = c ++ "-" ++ String.fromInt n
                    , x = 0
                    , y = 0
                    , width = 0
                    , height = 0
                    , selected = False
                    , isRange = False
                    , content = "Item" ++ String.fromInt n
                    }

                new1 =
                    Array.initialize 50 (initItem "scroll-list1")

                new2 =
                    Array.initialize 100 (initItem "space-list")
            in
            ( { model | items = Array.toList new1 }, getElementOfList )

        GetElements (Ok items) ->
            ( { model | items = items }, Cmd.none )

        GetElements (Err err) ->
            ( model, Cmd.none )

        ScrollList ->
            ( model, Task.attempt GetScrollPosition (Dom.getViewportOf "scroll-list1") )

        GetScrollPosition (Ok v) ->
            ( { model | listScrollY = v.viewport.y }, Cmd.none )

        GetScrollPosition (Err error) ->
            ( model, Cmd.none )

        LoadListItem index ->
            ( model, Cmd.none )


toStatic : Model -> Model
toStatic model =
    let
        newItems =
            List.map
                (\item -> { item | selected = False, isRange = False })
                model.items
    in
    { model | items = newItems }


updateByDragStop : Model -> Model
updateByDragStop model =
    case model.dragState of
        Static ->
            model

        Fitting _ _ ->
            -- DragStop中にDragMoveがきてしまう
            model

        Moving selIndex selItem ->
            let
                modelToFitting sel m =
                    { m
                        | startCursorX = 0
                        , startCursorY = 0
                        , selCenterY = 0
                        , dir = 0
                        , dragState = Fitting selIndex sel
                    }

                -- アイテムを上にドロップしたときは、その範囲のものは下に移動させる
                dropUp : Bool -> List Item -> ( List Item, Item )
                dropUp dropped list =
                    case list of
                        [] ->
                            ( [], selItem )

                        item :: items ->
                            case ( dropped, item.isRange ) of
                                ( False, True ) ->
                                    -- itemの上にドロップ
                                    let
                                        ( rs, sel ) =
                                            dropUp True (item :: items)

                                        dropItem =
                                            { selItem | y = item.y, selected = True }
                                    in
                                    ( dropItem :: rs, dropItem )

                                ( True, True ) ->
                                    -- 範囲内のものは下に移動
                                    let
                                        ( rs, sel ) =
                                            dropUp dropped items
                                    in
                                    ( { item | y = item.y + selItem.height, isRange = False } :: rs, sel )

                                ( _, _ ) ->
                                    case item.selected of
                                        True ->
                                            case dropped of
                                                True ->
                                                    -- 消す
                                                    dropUp dropped items

                                                False ->
                                                    -- 同じ場所にドロップ?
                                                    let
                                                        ( rs, sel ) =
                                                            dropUp True items
                                                    in
                                                    ( item :: rs, sel )

                                        False ->
                                            -- 変化なし
                                            let
                                                ( rs, sel ) =
                                                    dropUp dropped items
                                            in
                                            ( item :: rs, sel )

                -- アイテムを下にドロップしたときは、その範囲のものは上に移動させる
                dropDown : Bool -> List Item -> ( List Item, Item )
                dropDown deleted list =
                    case list of
                        [] ->
                            ( [], selItem )

                        item :: items ->
                            case deleted of
                                False ->
                                    case item.selected of
                                        True ->
                                            if items == [] then
                                                ( [ item ], item )

                                            else
                                                -- 消す
                                                dropDown True items

                                        False ->
                                            -- 変化なし
                                            let
                                                ( rs, sel ) =
                                                    dropDown deleted items
                                            in
                                            ( item :: rs, sel )

                                True ->
                                    case item.isRange of
                                        True ->
                                            if items == [] then
                                                -- 範囲内のものを上に移動して、ドロップも
                                                let
                                                    dropItem =
                                                        { selItem | y = item.y, selected = True }
                                                in
                                                ( { item | y = item.y - selItem.height, isRange = False } :: dropItem :: [], dropItem )

                                            else
                                                -- 範囲内のものは上に移動
                                                let
                                                    ( rs, sel ) =
                                                        dropDown deleted items
                                                in
                                                ( { item | y = item.y - selItem.height, isRange = False } :: rs, sel )

                                        False ->
                                            -- ドロップ
                                            let
                                                dropItem =
                                                    { selItem | y = item.y - selItem.height, selected = True }
                                            in
                                            ( dropItem :: item :: items, dropItem )
            in
            if model.dir == 1 then
                let
                    ( rs, sel ) =
                        dropUp False model.items
                in
                { model
                    | items = rs
                }
                    |> modelToFitting sel

            else if model.dir == -1 then
                let
                    ( rs, sel ) =
                        dropDown False model.items
                in
                { model
                    | items = rs
                }
                    |> modelToFitting sel

            else
                model


updateByDragMove : Int -> Float -> Float -> Model -> Model
updateByDragMove index x y model =
    let
        dir =
            if y < model.selCenterY then
                1

            else if model.selCenterY < y then
                -1

            else
                0

        updateToIsRange : Int -> List Item -> List Item
        updateToIsRange i list =
            case list of
                [] ->
                    []

                item :: items ->
                    let
                        itemCenterY =
                            (item.y - model.listScrollY) + item.height / 2

                        isRange =
                            if model.dir == 1 then
                                y < itemCenterY && itemCenterY < model.selCenterY

                            else if model.dir == -1 then
                                model.selCenterY < itemCenterY && itemCenterY < y

                            else
                                False
                    in
                    { item | isRange = isRange, selected = index == i } :: updateToIsRange (i + 1) items
    in
    { model
        | pageX = x
        , pageY = y
        , dir = dir
        , items = updateToIsRange 0 model.items
    }


scrollTo : String -> Float -> Cmd Msg
scrollTo id y =
    let
        step =
            8

        sub : Float -> Task Dom.Error ()
        sub cy =
            Dom.setViewportOf id 0 cy
                |> Task.andThen (\_ -> Process.sleep 10)
                |> Task.andThen
                    (if cy < y then
                        \_ -> sub (cy + step)

                     else
                        Task.succeed
                    )
    in
    Task.attempt (\_ -> NoOp) (sub step)



-- VIEW


goTransform : Int -> Html.Attribute Msg
goTransform c =
    if c > 0 then
        style "transform" "translateY(50px)"

    else
        style "transform" "translateY(0px)"


viewItem : Item -> Html Msg
viewItem item =
    div
        [ class "list-item"
        ]
        [ text item.content ]


viewDragItem_ : Model -> Html Msg
viewDragItem_ model =
    case model.dragState of
        Static ->
            div [] []

        Moving index item ->
            viewDragItem model item

        Fitting index item ->
            viewDragItem model item


viewDragItem : Model -> Item -> Html Msg
viewDragItem model item =
    div
        (List.concat
            [ [ class "drag"
              , style "position" "absolute"
              , style "top" (String.fromFloat (item.y - model.listScrollY) ++ "px")
              , style "left" (String.fromFloat item.x ++ "px")
              , style "width" (String.fromFloat item.width ++ "px")
              , style "height" (String.fromFloat item.height ++ "px")
              ]
            , case model.dragState of
                Moving _ _ ->
                    [ style "transform"
                        ("translate3d("
                            ++ String.fromFloat (model.pageX - model.startCursorX)
                            ++ "px,"
                            ++ String.fromFloat (model.pageY - model.startCursorY)
                            ++ "px,0px)"
                        )
                    ]

                _ ->
                    []
            , case model.dragState of
                Fitting _ _ ->
                    [ style "transition-duration" "300ms" ]

                _ ->
                    []
            ]
        )
        [ viewItem item ]


myList : String -> List Item -> Model -> List ( String, Html.Html Msg )
myList prefix items model =
    case model.dragState of
        Moving selIndex selItem ->
            let
                sub : Int -> Item -> ( String, Html.Html Msg )
                sub index item =
                    ( item.id
                    , li
                        (List.append
                            [ id item.id
                            , style "pointer-events" "none"
                            , style "user-select" "none"
                            , style "visibility"
                                (case ( model.dragState, index == selIndex ) of
                                    ( Moving _ _, True ) ->
                                        "hidden"

                                    ( _, _ ) ->
                                        "visible"
                                )
                            ]
                            (case ( model.dragState, item.isRange ) of
                                ( Moving _ _, True ) ->
                                    [ class "slick"
                                    , style "transform" ("translateY(" ++ String.fromFloat (selItem.height * model.dir) ++ "px)")
                                    , style "transition-duration" "300ms"
                                    ]

                                ( _, _ ) ->
                                    [ class "static"
                                    , style "transition-duration" "300ms"
                                    ]
                            )
                        )
                        [ viewItem item ]
                    )
            in
            List.indexedMap sub items

        Static ->
            let
                sub : Int -> Item -> ( String, Html.Html Msg )
                sub index item =
                    ( item.id
                    , li
                        [ id item.id
                        , on "mousedown" (D.map2 (DragStart index item) (D.field "pageX" D.float) (D.field "pageY" D.float))
                        , style "user-select" "none"
                        , style "transition-duration" "300ms"
                        ]
                        [ viewItem item ]
                    )
            in
            List.indexedMap sub items

        Fitting indexF itemF ->
            let
                sub : Int -> Item -> ( String, Html.Html Msg )
                sub index item =
                    ( item.id
                    , li
                        [ id item.id
                        , style "visibility"
                            (if item.selected then
                                "hidden"

                             else
                                "visible"
                            )
                        , style "user-select" "none"
                        ]
                        [ viewItem item ]
                    )
            in
            List.indexedMap sub items


view : Model -> Html Msg
view model =
    div []
        [ div [ class "element-panel" ]
            [ h2 []
                [ text "ドラッグソート可能なリスト" ]
            , ul []
                [ li [] [ text "まずinitListを押してください。" ]
                , li [] [ text "次にgetElementを押してください" ]
                , li [] [ text "ドラッグ可能になるはずです" ]
                ]
            ]
        , div []
            [ Keyed.ul
                [ class "scroll-list"
                , id "scroll-list1"
                , style "max-height" "30vh"
                , style "overflow" "auto"
                , on "scroll" (D.succeed ScrollList)
                ]
                (myList "scroll-list-1" model.items model)
            , viewDragItem_ model
            ]
        , h1 [ style "position" "absolute", style "top" "2000px" ]
            [ text "hoge" ]
        , div
            [ style "position" "fixed", style "bottom" "5px" ]
            [ button
                [ style "left" "0px"
                , onClick ClickGetElement
                ]
                [ text "getElement" ]
            , text ("x:" ++ String.fromFloat model.pageX ++ ", y:" ++ String.fromFloat model.pageY)
            , button [ onClick ClickScroll ]
                [ text "scroll" ]
            , button [ onClick InitList ]
                [ text "initList" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.dragState of
            Moving index item ->
                Sub.batch
                    [ BE.onMouseMove (D.map3 DragMove decodeButtons (D.field "pageX" D.float) (D.field "pageY" D.float))
                    , BE.onMouseUp (D.map2 DragStop (D.field "pageX" D.float) (D.field "pageY" D.float))
                    ]

            _ ->
                Sub.none
        ]


decodeFraction : D.Decoder Float
decodeFraction =
    D.map2 (/)
        (D.field "pageX" D.float)
        (D.at [ "currentTarget", "defaultView", "innerWidth" ] D.float)


decodeButtons : D.Decoder Bool
decodeButtons =
    D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)
