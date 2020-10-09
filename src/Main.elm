module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, a, button, div, input, p, pre, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (Cmd)
import Random
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
    , cardsNumber : Int
    , guessed : Set Int
    , selected : Selected
    , match : Bool
    , allGuessed : Bool
    , score : Int
    , cardFace: CardFace
    }


type CardFace
    = IndexCardFace
    | StringCardFace


defaultCardNumber =
    3


debug =
    True


init : () -> ( Model, Cmd Msg )
init _ =
    ( resetModel defaultCardNumber
    , Random.generate GotRandom (randomPicker defaultCardNumber)
    )


generateCards x =List.range 0 ((x*2) - 1)
    -- List.concat (List.map (\y -> [ y, y ]) (List.range 0 (x - 1)))


randomPicker : Int -> Random.Generator (List ( Int, Int ))
randomPicker x =
    Random.list (x * 2) (Random.pair (Random.int 0 (x * 2)) (Random.int 0 (x * 2)))


resetModel x =
    { cards = Array.fromList []
    , cardsNumber = x
    , guessed = Set.empty
    , match = False
    , selected = SelectedNone
    , allGuessed = False
    , score = 0
    , cardFace = StringCardFace
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

valueFromId x = x // 2

-- UPDATE


type Msg
    = OpenCard Int
    | CheckCards
    | Reset
    | GotRandom (List ( Int, Int ))
    | ChangeNumber String


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
                                          valueFromId  first == valueFromId second

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
            ( resetModel model.cardsNumber, Random.generate GotRandom (randomPicker model.cardsNumber) )

        GotRandom y ->
            ( { model | cards = shuffle y (Array.fromList (generateCards model.cardsNumber)) }, Cmd.none )

        ChangeNumber s ->
            case String.toInt s of
                Just n ->
                    ( resetModel n, Random.generate GotRandom (randomPicker n) )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Score: " ++ String.fromInt model.score) ]
        , div [] (Array.indexedMap (viewButton model.selected model.guessed model.cardFace) model.cards |> Array.toList)
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
        , input
            [ Html.Attributes.value (String.fromInt model.cardsNumber)
            , onInput ChangeNumber
            , Html.Attributes.type_ "number"
            ]
            []
        ]


viewButton : Selected -> Set Int ->  CardFace-> Int -> Int -> Html Msg
viewButton selected guessed cardFace idx v  =
    div []
        [ div [ onClick (OpenCard idx) ]
            [ text
                (if Set.member idx guessed then
                    if debug == False then
                        " "

                    else
                        "+ " ++ viewFace cardFace v

                 else
                    case selected of
                        SelectedNone ->
                            if debug == False then
                                "X"

                            else
                                "- " ++ viewFace cardFace v

                        --    String.fromInt v -- to debug
                        SelectedOne x ->
                            if x == idx then
                                viewFace cardFace v

                            else if debug == False then
                                "X"

                            else
                                "- " ++ viewFace cardFace v

                        SelectedTwo x y ->
                            if x == idx || y == idx then
                                viewFace cardFace v

                            else if debug == False then
                                "X"

                            else
                                "- " ++ viewFace cardFace v
                )
            ]
        ]

viewFace cardFace v = case cardFace of
  IndexCardFace -> String.fromInt (valueFromId v)
  StringCardFace -> Maybe.withDefault "BAD" (Array.get v (Array.fromList ["Poland","Warsaw","France","Paris","UK","London","Germany","Berlin"]))