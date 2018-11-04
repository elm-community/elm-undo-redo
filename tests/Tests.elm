module Tests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, test)
import UndoList exposing (UndoList, Msg(..))
import UndoList.Encode as Encode
import UndoList.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


encoding_and_decoding_inverse : Test
encoding_and_decoding_inverse =
    Test.fuzz (undoListFuzzer Fuzz.int) "Encoding and decoding functions are inverse operations" <|
        \undolist ->
            undolist
                |> encodeThenDecode (Json.Encode.int) (Json.Decode.int)
                |> Expect.equal (Ok undolist)


undolist_length_atleastone : Test
undolist_length_atleastone =
    Test.fuzz (undoListFuzzer Fuzz.int) "The length of an undo list is at least one" <|
        \undolist ->
            UndoList.length undolist
                |> Expect.atLeast 1


redo_does_not_change_length : Test
redo_does_not_change_length =
    Test.fuzz (undoListFuzzer Fuzz.int) "Redo does not change the length of an undo list" <|
        \undolist ->
            UndoList.redo undolist
                |> UndoList.length
                |> Expect.equal (UndoList.length undolist)


undo_does_not_change_length : Test
undo_does_not_change_length =
    Test.fuzz (undoListFuzzer Fuzz.int) "Undo does not change the length of an undo list" <|
        \undolist ->
            UndoList.undo undolist
                |> UndoList.length
                |> Expect.equal (UndoList.length undolist)


forget_produces_empty_past : Test
forget_produces_empty_past =
    Test.fuzz (undoListFuzzer Fuzz.int) "After forgetting the past, the past of the undo list is empty" <|
        \undolist ->
            UndoList.forget undolist
                |> UndoList.lengthPast
                |> Expect.equal 0


new_produces_empty_future : Test
new_produces_empty_future =
    Test.fuzz2 (Fuzz.float) (undoListFuzzer Fuzz.float) "Adding a new state yields an empty future" <|
        \item undolist ->
            UndoList.new item undolist
                |> UndoList.lengthFuture
                |> Expect.equal 0


new_adds_one_length_past : Test
new_adds_one_length_past =
    Test.fuzz2 (Fuzz.string) (undoListFuzzer Fuzz.string) "Adding a new state adds one element to the past" <|
        \item undolist ->
            UndoList.new item undolist
                |> UndoList.lengthPast
                |> Expect.equal (UndoList.lengthPast undolist + 1)


undo_and_redo_inverse : Test
undo_and_redo_inverse =
    Test.fuzz (undoListFuzzer Fuzz.int) "Undo and redo are inverse operations" <|
        \undolist ->
            undolist
                |> undo_redo
                |> Expect.equal undolist


redo_and_undo_inverse : Test
redo_and_undo_inverse =
    Test.fuzz (undoListFuzzer Fuzz.int) "Redo and undo are inverse operations" <|
        \undolist ->
            undolist
                |> redo_undo
                |> Expect.equal undolist


new_then_undo_yields_same_present : Test
new_then_undo_yields_same_present =
    Test.fuzz2 (Fuzz.string) (undoListFuzzer Fuzz.string) "Calling new then undo preserves the original present state" <|
        \item undolist ->
            undolist
                |> UndoList.new item
                |> UndoList.undo
                |> .present
                |> Expect.equal undolist.present


reset_equivalent_fresh_oldest : Test
reset_equivalent_fresh_oldest =
    Test.fuzz (undoListFuzzer Fuzz.int) "Resetting an undo list is equivalent to creating an undo list with the oldest state" <|
        \undolist ->
            UndoList.reset undolist
                |> Expect.equal (fresh_oldest undolist)


state_machine_length : Test
state_machine_length =
    Test.fuzz2 (Fuzz.list <| msgFuzzer Fuzz.int) (undoListFuzzer Fuzz.int) "State Machine is consistent with respect to length" <|
        \msgs undolist ->
            state_machine_update msgs undolist
                |> Expect.equal (state_machine_step msgs undolist)



-- Test Helpers


msgFuzzer : Fuzzer a -> Fuzzer (Msg a)
msgFuzzer fuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.constant Reset )
        , ( 1, Fuzz.constant Forget )
        , ( 6, Fuzz.constant Undo )
        , ( 6, Fuzz.constant Redo )
        , ( 6, Fuzz.map New fuzzer )
        ]


undoListFuzzer : Fuzzer a -> Fuzzer (UndoList a)
undoListFuzzer fuzzer =
    Fuzz.map3 UndoList
        (Fuzz.list fuzzer)
        fuzzer
        (Fuzz.list fuzzer)


encodeThenDecode : (state -> Value) -> Decoder state -> UndoList state -> Result Json.Decode.Error (UndoList state)
encodeThenDecode encoder decoder undolist =
    let
        encoded =
            undolist
                |> UndoList.map encoder
                |> Encode.undolist

        decoded =
            Json.Decode.decodeValue (Decode.undolist decoder) encoded
    in
        decoded


undo_redo : UndoList state -> UndoList state
undo_redo undolist =
    if UndoList.hasPast undolist then
        UndoList.undo undolist
            |> UndoList.redo
    else
        undolist


redo_undo : UndoList state -> UndoList state
redo_undo undolist =
    if UndoList.hasFuture undolist then
        UndoList.redo undolist
            |> UndoList.undo
    else
        undolist


fresh_oldest : UndoList state -> UndoList state
fresh_oldest undolist =
    undolist.past
        |> List.reverse
        |> List.head
        |> Maybe.withDefault undolist.present
        |> UndoList.fresh


state_machine_update : List (Msg Int) -> UndoList Int -> List ( Int, Int )
state_machine_update msgs undolist =
    msgs
        |> List.map update
        |> pipe undolist
        |> List.map (\l -> ( UndoList.lengthPast l, UndoList.lengthFuture l ))


state_machine_step : List (Msg Int) -> UndoList Int -> List ( Int, Int )
state_machine_step msgs undolist =
    msgs
        |> List.map step
        |> pipe ( UndoList.lengthPast undolist, UndoList.lengthFuture undolist )


update : Msg a -> UndoList a -> UndoList a
update msg undolist =
    case msg of
        Reset ->
            UndoList.reset undolist

        Redo ->
            UndoList.redo undolist

        Undo ->
            UndoList.undo undolist

        Forget ->
            UndoList.forget undolist

        New n ->
            UndoList.new n undolist


step : Msg a -> ( Int, Int ) -> ( Int, Int )
step msg ( pastLen, futureLen ) =
    case msg of
        Reset ->
            ( 0, 0 )

        Redo ->
            if futureLen == 0 then
                ( pastLen, futureLen )
            else
                ( pastLen + 1, futureLen - 1 )

        Undo ->
            if pastLen == 0 then
                ( pastLen, futureLen )
            else
                ( pastLen - 1, futureLen + 1 )

        Forget ->
            ( 0, futureLen )

        New _ ->
            ( pastLen + 1, 0 )


pipe : state -> List (state -> state) -> List state
pipe state msgs =
    case msgs of
        [] ->
            [ state ]

        f :: fs ->
            state :: pipe (f state) fs
