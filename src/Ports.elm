port module Ports exposing (..)

import Json.Decode
import Json.Encode


port saveUser : Json.Encode.Value -> Cmd msg


port getUser : () -> Cmd msg


port restoreUser : (Json.Decode.Value -> msg) -> Sub msg
