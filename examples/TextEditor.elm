module TextEditor exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Event
import Html.Attributes as Attr
import UndoList exposing (UndoList)


-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    UndoList { content : String }


init : Model
init =
    UndoList.fresh { content = "" }



-- Update


type Msg
    = UpdateContent String
    | Undo
    | Redo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateContent str ->
            UndoList.new { content = str } model

        Undo ->
            UndoList.undo model

        Redo ->
            UndoList.redo model



-- View


view : Model -> Html Msg
view model =
    let
        button text msg =
            Html.button
                [ Event.onClick msg
                , Attr.style "width" "8em"
                , Attr.style "height" "3em"
                , Attr.style "font-size" "14pt"
                ]
                [ Html.text text ]

        undoButton =
            button "Undo" Undo

        redoButton =
            button "Redo" Redo

        title =
            Html.span
                [ Attr.style "font-size" "16pt" ]
                [ Html.text "Simple Text Area with Undo/Redo support" ]

        headerArea =
            Html.div
                [ Attr.style "display" "flex"
                , Attr.style "justify-content" "space-between"
                , Attr.style "align-items" "center"
                ]
                [ undoButton
                , title
                , redoButton
                ]

        textArea =
            Html.textarea
                [ Event.onInput UpdateContent
                , Attr.value model.present.content
                , Attr.placeholder "Enter text here..."
                , Attr.style "flex" "1"
                , Attr.style "font-size" "24pt"
                , Attr.style "font-family" "Helvetica Neue, Helvetica, Arial, sans-serif"
                , Attr.style "resize" "none"
                ]
                []
    in
        Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "margin" "0"
            , Attr.style "padding" "0"
            , Attr.style "width" "100vw"
            , Attr.style "height" "100vh"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            ]
            [ headerArea
            , textArea
            ]
