module UndoList.Encode exposing (undolist, msg)

{-| Encode UndoList submodule.

Provides JSON encoders for Timelines and UndoList Messages.


# Encoders

@docs undolist, msg

-}

import Json.Encode as Encode exposing (Value)
import UndoList exposing (Msg(..), UndoList)


{-| Encode an undolist of JSON values.
Best paired with the `map` function from UndoList.

    encodeUndoList stateEncoder =
        UndoList.map stateEncoder >> undolist

-}
undolist : UndoList Value -> Value
undolist { past, present, future } =
    Encode.object
        [ ( "past", Encode.list identity past )
        , ( "present", present )
        , ( "future", Encode.list identity future )
        ]


{-| Encode an UndoList Msg of JSON values.
Best paired with the `mapMsg` function from UndoList.

    encodeMsg msgEncoder =
        UndoList.mapMsg msgEncoder >> msg

-}
msg : Msg Value -> Value
msg wrapperMessage =
    case wrapperMessage of
        Reset ->
            Encode.string "Reset"

        Redo ->
            Encode.string "Redo"

        Undo ->
            Encode.string "Undo"

        Forget ->
            Encode.string "Forget"

        New value ->
            Encode.object [ ( "New", value ) ]
