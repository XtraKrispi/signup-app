module Api.Request exposing (..)

import Codec
import Data.Codec
import Data.Types exposing (Sheet)
import Json.Decode as Decode
import RemoteData exposing (WebData)
import RemoteData.Http


getSheets : String -> (WebData (List Sheet) -> msg) -> Cmd msg
getSheets baseUrl msg =
    let
        url =
            baseUrl ++ "/sheets"
    in
    Data.Codec.sheetCodec
        |> Codec.decoder
        |> Decode.list
        |> RemoteData.Http.get url msg
