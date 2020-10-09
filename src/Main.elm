module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict
import Html exposing (Html, a, button, div, img, input, p, pre, text)
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
    , cardFace : CardFace
    }


type CardFace
    = IndexCardFace
    | StringCardFace String
    | ImgCardFace String


defaultCardNumber =
    3


debug =
    True


init : () -> ( Model, Cmd Msg )
init _ =
    ( resetModel defaultCardNumber
    , Random.generate GotRandom (randomPicker defaultCardNumber)
    )


generateCards x =
    List.range 0 ((x * 2) - 1)



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
    , cardFace = ImgCardFace "Github"
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


valueFromId x =
    x // 2



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
                                            valueFromId first == valueFromId second

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


viewButton : Selected -> Set Int -> CardFace -> Int -> Int -> Html Msg
viewButton selected guessed cardFace idx v =
    div []
        [ div [ onClick (OpenCard idx) ]
            [ (if Set.member idx guessed then
                viewFace cardFace v

               else
                case selected of
                    SelectedNone ->
                        "https://avatars3.githubusercontent.com/u/15275588?s=400&v=4"

                    -- viewFace cardFace v
                    --    String.fromInt v -- to debug
                    SelectedOne x ->
                        if x == idx then
                            viewFace cardFace v

                        else
                            "https://avatars3.githubusercontent.com/u/15275588?s=400&v=4"

                    SelectedTwo x y ->
                        if x == idx || y == idx then
                            viewFace cardFace v

                        else
                            "https://avatars3.githubusercontent.com/u/15275588?s=400&v=4"
              )
                |> (case cardFace of
                        ImgCardFace x ->
                            showImg

                        _ ->
                            text
                   )
            ]
        ]


showImg s =
    img [ Html.Attributes.src s ] []


stringLists =
    Dict.fromList
        [ ( "Capitals", flattenToArray [ ( "Poland", "Warsaw" ), ( "France", "Paris" ), ( "UK", "London" ), ( "Germany", "Berlin" ) ] )
        , ( "Spanish", flattenToArray [ ( "aqua", "water" ), ( "fuego", "flame" ), ( "hola", "hello" ), ( "si", "yes" ) ] )
        , ( "Names", doubleAndToArray [ "Jan", "Lucas", "Ewa", "Kasia" ] )
        ]


picsLists =
    Dict.fromList
        [ ( "Github"
          , doubleAndToArray
                [ "https://avatars1.githubusercontent.com/u/1197854?s=400&u=c24dd31b89b1b9f035368dec44948c2d03828a8d&v=4"
                , "https://avatars0.githubusercontent.com/u/5942539?s=400&u=cd921bcb4886c0aa68692bb4c24c4dd20d05a949&v=4"
                , "https://avatars2.githubusercontent.com/u/17748441?s=400&u=ffe398923e87e80206af2268ac37341a2817a3d0&v=4"
                , "https://avatars0.githubusercontent.com/u/59838385?s=400&u=dea71e1e51c13242bc1e7fc5be82b3785c8ca909&v=4"
                , "https://avatars0.githubusercontent.com/u/19223482?s=400&u=317e1ab608e1c78eeac579541304990b288e9fdc&v=4"
                ]
          )
        ]


flattenToArray : List ( x, x ) -> Array x
flattenToArray listOfPairs =
    Array.fromList (List.concat (List.map (\( x, y ) -> [ x, y ]) listOfPairs))


doubleAndToArray : List x -> Array x
doubleAndToArray listOfFaces =
    Array.fromList (List.concat (List.map (\x -> [ x, x ]) listOfFaces))


viewFace cardFace v =
    case cardFace of
        IndexCardFace ->
            String.fromInt (valueFromId v)

        StringCardFace stringList ->
            case Dict.get stringList stringLists of
                Just correctList ->
                    Maybe.withDefault "BAD" (Array.get v correctList)

                Nothing ->
                    "--ERROR--"

        ImgCardFace imgList ->
            case Dict.get imgList picsLists of
                Just correctList ->
                    Maybe.withDefault "BAD" (Array.get v correctList)

                Nothing ->
                    "--ERROR--"



-- Maybe.withDefault "BAD" (Array.get v (Array.fromList [ "Poland", "Warsaw", "France", "Paris", "UK", "London", "Germany", "Berlin" ]))
