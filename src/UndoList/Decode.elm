module UndoList.Decode exposing (undolist, msg)

{-| Decode UndoList submodule.

Provides JSON decoders for Timelines and UndoList Messages.


# Decoders

@docs undolist, msg

-}

import Json.Decode as Decode exposing (Decoder)
import UndoList exposing (Msg(..), UndoList)


{-| Decode an undo-list given a decoder of state.

    import Json.Decode

    json : String
    json = """{
        "past": [ 1, 2 ],
        "present": 3,
        "future": [ 4, 5 ]
    }"""

    Json.Decode.decodeString (undolist Json.Decode.int) json
    --> Ok { past = [ 1, 2 ], present = 3, future = [ 4, 5 ] }
-}
undolist : Decoder state -> Decoder (UndoList state)
undolist state =
    Decode.map3 UndoList
        (Decode.field "past" (Decode.list state))
        (Decode.field "present" state)
        (Decode.field "future" (Decode.list state))


{-| Decode an undo-list msg given a decoder of messages.

    import Json.Decode
    import UndoList exposing (Msg(..))

    Json.Decode.decodeString (msg Json.Decode.string) "{ \"New\": \"Hello!\" }"
    --> Ok (New "Hello!")

    json : String
    json = """[ "Reset", "Redo", "Undo", "Forget", { "New": 1 } ]"""

    Json.Decode.decodeString (Json.Decode.list <| msg Json.Decode.int) json
    --> Ok [ Reset, Redo, Undo, Forget, New 1 ]
-}
msg : Decoder msg -> Decoder (Msg msg)
msg decoder =
    let
        unionDecoder =
            Decode.string
                |> Decode.map decodeMsgString
                |> Decode.andThen fromResult
    in
        Decode.oneOf
            [ unionDecoder
            , Decode.map New (Decode.field "New" decoder)
            ]


fromResult : Result String a -> Decode.Decoder a
fromResult result =
    case result of
        Ok val ->
            Decode.succeed val

        Err reason ->
            Decode.fail reason


decodeMsgString : String -> Result String (Msg msg)
decodeMsgString str =
    if str == "Reset" then
        Ok Reset
    else if str == "Redo" then
        Ok Redo
    else if str == "Undo" then
        Ok Undo
    else if str == "Forget" then
        Ok Forget
    else
        Err (str ++ " is not a valid undolist message")
