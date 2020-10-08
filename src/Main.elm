module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, a, button, div, p, pre, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)
import Random


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
    , allGuessed : Bool
    , score : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( resetModel
    , Random.generate GotRandom randomPicker
    )

cardsCount = 2

generateCards x = List.concat( List.map (\y->[y,y]) (List.range 0 x))

randomPicker: Random.Generator (List (Int,Int))
randomPicker = Random.list (cardsCount*2) (Random.pair (Random.int 0 (cardsCount*2)) (Random.int 0 (cardsCount*2)))
resetModel =
    { cards =  (Array.fromList (generateCards cardsCount))
    , guessed = Set.empty
    , match = False
    , selected = SelectedNone
    , allGuessed = False
    , score = 0
    }


swapIndexes : ( Int, Int ) -> Array x -> Array x
swapIndexes ( i1, i2 ) a =
    case ( Array.get i1 a, Array.get i2 a ) of
        ( Just n1, Just n2 ) ->
            Array.set i1 n2 (Array.set i2 n1 a)

        _ ->
            a


shuffle : List ( Int, Int ) -> Array x -> Array x
shuffle s a =
    List.foldl swapIndexes a s



-- UPDATE


type Msg
    = OpenCard Int
    | CheckCards
    | Reset
    | GotRandom (List (Int,Int))


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
                            SelectedOne i
                , score = model.score + 1
            }
                |> update CheckCards

        CheckCards ->
            let
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
            if matched then
                let
                    newGuessed : Set Int
                    newGuessed =
                        case model.selected of
                            SelectedTwo x y ->
                                Set.union (Set.fromList [ x, y ]) model.guessed

                            _ ->
                                model.guessed

                    allGuessed =
                        Set.size newGuessed == Array.length model.cards
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

        Reset ->
            ( resetModel, Random.generate GotRandom randomPicker )

        GotRandom y -> ({model|cards=shuffle y model.cards} ,Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Score: " ++ String.fromInt model.score) ]
        , div [] (Array.indexedMap (viewButton model.selected model.guessed) model.cards |> Array.toList)
        , p []
            [ text
                (if model.match then
                    "They match"

                 else
                    "No match!"
                )
            ]
        , p []
            [ text
                (if model.allGuessed then
                    "You won!"

                 else
                    ""
                )
            ]
        , button [ onClick Reset ]
            [ text "Start Again!"
            ]
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
                        --    String.fromInt v -- to debug
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
