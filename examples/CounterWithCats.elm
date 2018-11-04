module CounterWithCats exposing (..)

import Browser
import Html exposing (Html, div, button, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import UndoList exposing (UndoList)
import Http
import Task
import Json.Decode as Json


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    UndoList
        { counter : Int
        , gifUrl : Maybe String
        }


type Msg
    = Increment
    | Undo
    | Redo
    | OnFetch (Result Http.Error String)


init : ( Model, Cmd msg )
init =
    ( UndoList.fresh { counter = 0, gifUrl = Nothing }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ present } as model) =
    case msg of
        Increment ->
            ( UndoList.new { present | counter = present.counter + 1 }
                model
            , getRandomGif "cats"
            )

        Undo ->
            ( UndoList.undo model, Cmd.none )

        Redo ->
            ( UndoList.redo model, Cmd.none )

        OnFetch (Ok newUrl) ->
            ( UndoList.mapPresent (\_ -> { present | gifUrl = Just newUrl })
                model
            , Cmd.none
            )

        OnFetch (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ]
            [ text "Increment" ]
        , button [ onClick Undo ]
            [ text "Undo" ]
        , button [ onClick Redo ]
            [ text "Redo" ]
        , div []
            [ text (String.fromInt model.present.counter) ]
        , div []
            (case model.present.gifUrl of
                Just gifUrl ->
                    [ img [ src gifUrl ] [] ]

                Nothing ->
                    [ text "Increment to display a GIF image" ]
            )
        ]



-- Taken from https://guide.elm-lang.org/architecture/effects/http.html


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Http.get url decodeGifUrl
            |> Http.send OnFetch


decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string
