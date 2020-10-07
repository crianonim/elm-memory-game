module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, p, pre, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Selected
    = SelectedNone
    | SelectedOne Int
    | SelectedTwo Int Int


type alias Model =
    { cards : Array Int
    , guessed : Set Int
    , selected : Selected
    , match : Bool
    , allGuessed: Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = Array.fromList [ 1, 2, 3, 3, 1, 2 ]
      , guessed = Set.empty
      , match = False
      , selected = SelectedNone
      , allGuessed = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OpenCard Int
    | CheckCards
    | ClearSelection
    | RemoveMatched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenCard i ->
            { model
                | selected =
                    case model.selected of
                        SelectedNone ->
                            SelectedOne i

                        SelectedOne x ->
                            if x == i then
                                SelectedOne x

                            else
                                SelectedTwo x i

                        SelectedTwo x y ->
                            SelectedTwo x y
            }
                |> update CheckCards

        CheckCards ->
            ( let
                matched =
                    case model.selected of
                        SelectedTwo x y ->
                            case Array.get x model.cards of
                                Nothing ->
                                    False

                                Just first ->
                                    case Array.get y model.cards of
                                        Nothing ->
                                            False

                                        Just second ->
                                            first == second

                        _ ->
                            False
              in
              { model
                | match = matched
              }
            , Cmd.none
            )

        ClearSelection ->
            { model | selected = SelectedNone } |> update CheckCards

        RemoveMatched ->
            if model.match then
                let
                    newGuessed : Set Int
                    newGuessed =
                        case model.selected of
                            SelectedTwo x y ->
                                Set.union (Set.fromList [ x, y ]) model.guessed

                            _ ->
                                model.guessed
                    allGuessed = (Set.size newGuessed) == (Array.length model.cards)
                in
                ( { model
                    | match = False
                    , selected = SelectedNone
                    , guessed = newGuessed
                    , allGuessed = allGuessed
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (Array.indexedMap (viewButton model.selected model.guessed) model.cards |> Array.toList)
        , p []
            [ text
                (if model.match then
                    "They match"

                 else
                    "No match!"
                )
            ]
        , button [ onClick ClearSelection ] [ text "Clear" ]
        , button [ onClick RemoveMatched ] [ text "Guess" ]
        , p [] [ text (if model.allGuessed then "You won!" else "")]

        ]


viewButton : Selected -> Set Int -> Int -> Int -> Html Msg
viewButton selected guessed idx v =
    div []
        [ button [ onClick (OpenCard idx) ]
            [ text
                (if Set.member idx guessed then
                    " " 

                 else
                    case selected of
                        SelectedNone ->
                            "X"

                        SelectedOne x ->
                            if x == idx then
                                String.fromInt v

                            else
                                "X"

                        SelectedTwo x y ->
                            if x == idx || y == idx then
                                String.fromInt v

                            else
                                "X"
                )
            ]
        ]