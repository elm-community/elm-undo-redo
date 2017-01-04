# Undo/redo in any Elm app

Add undo/redo to any Elm app with just a few lines of code!

Trying to add undo/redo in JS can be a nightmare. If anything gets mutated in
an unexpected way, your history can get corrupted. Elm is built from the ground
up around efficient, immutable data structures. That means adding support for
undo/redo is a matter of remembering the state of your app at certain times.
Since there is no mutation, there is no risk of things getting corrupted. Given
immutability lets you do structural sharing within data structures, it also
means these snapshots can be quite compact!


## How it works

The library is centered around a single data structure, the `UndoList`.

```elm
type alias UndoList state =
    { past: List state
    , present: state
    , future: List state
    }
```

An `UndoList` contains a list of past states, a present state, and a list of
future states. By keeping track of the past, present, and future states, undo
and redo become just a matter of sliding the present around a bit.


## Example

### Initial counter app

We will start with a very simple counter application. There is a button, and
when it is clicked, a counter is incremented.

```elm
-- BEFORE
import Html exposing (div, button, text)
import Html.Events exposing (onClick)

main =
    Html.beginnerProgram
        { model = 0
        , view = view
        , update = update
        }

type alias Model = Int

type Msg = Increment

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick Increment ]
            [ text "Increment" ]
        , div
            []
            [ text (toString model) ]
        ]
```

### Adding undo

Suppose that further down the line we decide it would be nice to have an undo
button.

The next code block is the same program updated to use the `UndoList` module to
add this functionality. It is in one big block because it is mostly the same as
the original, and we will go into the differences afterwards.

```elm
-- AFTER
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import UndoList exposing (UndoList)

main =
    Html.beginnerProgram
        { model = UndoList.fresh 0
        , view = view
        , update = update
        }

type alias Model
    = UndoList Int

type Msg
    = Increment
    | Undo

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            UndoList.new (model.present + 1) model

        Undo ->
            UndoList.undo model

view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick Increment ]
            [ text "Increment" ]
        , button
            [ onClick Undo ]
            [ text "Undo" ]
        , div
            []
            [ text (toString model) ]
        ]
```

Here are the differences:
- the `Model` type changed from `Int` to `UndoList Int`
- the `Msg` type now has a new constructor `Undo`
- the `update` function now cares for this new `Undo` message in the pattern matching
- a `button` was added to the `view` function. It sends the `Undo` message

Adding redo functionality is quite the same. You can find by yourself as an exercise, or look at the
[counter example](./examples/Counter.elm).

### Usage with commands

When you use `Html.program` instead of `Html.beginnerProgram` as above, you can use commands
in your `update` function.

Look at the [counter with cats example](./examples/CounterWithCats.elm) which loads a GIF image whenever you increment
the counter, with undo/redo even with asynchronous operations.

## More Details

This API is designed to work really nicely with
[The Elm Architecture](http://guide.elm-lang.org/architecture/index.html).

It has a lot more cool stuff, so read the [docs](http://package.elm-lang.org/packages/elm-community/undo-redo/latest).
