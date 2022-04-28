module Pages.MathVisualizerQ2MCQ exposing (Model, Msg(..), QuestionType(..), contentPanel, defaultColors, drawChoice, drawQuestion, getAnswer, getChoices, getColor, getIndex, getNewQuestion, getQuestion, getQuestionText, init, lenlist, main, merge, myShapes, one2len, qbank, randIndex, shuffle, toString, update, view)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


-- QuestionType = question choices answer


type QuestionType
    = Question String (List String) String


qbank =
    [ Question "A 300 kg cable is 100 m long and hangs vertically from the top of a tall building. \nHow much work is required to lift the cable to the top of the building?\n(g = 10m/s^2)"
        [ "0 J"
        , "100000 J"
        , "300000 J"
        , "200 J"
        ]
        "100000 J"
    , Question "A 80 kg cable is 160 m long and hangs vertically from the top of a tall building. \nHow much work is required to lift the cable to the top of the building?\n(g = 10m/s^2)"
        [ "1489 J"
        , "16000 J"
        , "6400 J"
        , "64000 J"
        , "Not enough information."
        ]
        "64000 J"
    , Question "A 200 kg cable is 100 m long and hangs vertically from the top of a tall building. \nHow much work is required to pull up only 20 feet of the cable?\n(g = 10m/s^2)"
        [ "100 J"
        , "20000 J"
        , "36000 J"
        , "5000 J"
        ]
        "36000 J"
    ]


contentPanel model =
    roundedRect 190 114 5
        |> filled (getColor 1 model.themeColors)
        |> addOutline (solid 0.4) black
        |> move ( 0, -6.25 )



-- function to show the message to help us understand our program


toString msg =
    case msg of
        Tick t _ ->
            "Tick " ++ String.fromFloat t

        NewQuestion n ->
            "NewNumber: TEST"

        NextQuestion _ ->
            "Next"

        NewIndex i ->
            "Index"

        GetIndex ->
            "Get Index"


myShapes model =
    let
        question =
            model.question
    in
    [ contentPanel model
    , roundedRect 155 25 5
        |> outlined (solid 0.4) black
        |> move ( -17.5, -50.5 )
    , roundedRect 80 70 4
        |> outlined (solid 0.3) black
        |> move ( 40, 3 )
    ]
        ++ (List.concat <|
                [ drawQuestion question model
                , List.indexedMap (\i choice -> drawChoice choice i model) (getChoices model.index question)
                , [ text ("Score : " ++ String.fromInt model.score) |> sansserif |> size 5 |> centered |> filled (getColor 8 model.themeColors) |> move ( 80, 42 ) ]
                , [ text ("Total Correct : " ++ String.fromInt model.totalcorrect) |> sansserif |> size 5 |> centered |> filled (getColor 8 model.themeColors) |> move ( -70, 42 ) ]
                , [ text ("Total Incorrect : " ++ String.fromInt model.totalincorrect) |> sansserif |> size 5 |> centered |> filled (getColor 8 model.themeColors) |> move ( 5, 42 ) ]
                ]
           )


drawQuestion question model =
    let
        lines =
            String.lines <| getQuestionText question
    in
    List.indexedMap
        (\i line ->
            text line |> sansserif |> size 3.5 |> filled (getColor 8 model.themeColors) |> move ( -92, -44 - toFloat i * 5 )
        )
        lines


drawChoice choice idx model =
    [ roundedRect 70 8 2 |> filled (getColor 5 model.themeColors)
    , text choice |> sansserif |> centered |> size 5 |> filled (getColor 8 model.themeColors) |> move ( 0, -2 )
    ]
        |> group
        |> move ( 40, 30 - 13 * toFloat idx )
        |> notifyTap (NextQuestion choice)
        |> notifyTap GetIndex


getQuestion i questions =
    let
        el =
            List.head (List.drop i questions)
    in
    case el of
        Just q ->
            q

        Nothing ->
            Question "Nothing" [] "Nothing"


one2len : Model -> Random.Generator Int
one2len model =
    Random.int 0 (lenlist model)


getNewQuestion : Cmd Msg
getNewQuestion =
    Random.generate NewQuestion (one2len init)


lenlist model =
    List.length model.questions - 1


getQuestionText : QuestionType -> String
getQuestionText (Question question choices answer) =
    question


getChoices : Int -> QuestionType -> List String
getChoices idx (Question question choices answer) =
    shuffle choices idx


getAnswer : QuestionType -> String
getAnswer (Question question choices answer) =
    answer


randIndex =
    Random.int 0 5


getIndex =
    Random.generate NewIndex randIndex


shuffle : List a -> Int -> List a
shuffle lst idx =
    case lst of
        [] ->
            []

        [ a ] ->
            [ a ]

        _ ->
            let
                len =
                    List.length lst

                cutPoint =
                    modBy len idx

                left =
                    List.take cutPoint lst

                right =
                    List.drop cutPoint lst
            in
            merge right left



--merge (shuffle left idx) (shuffle right idx)


merge left right =
    case ( left, right ) of
        ( l1 :: lmore, _ ) ->
            l1 :: merge right lmore

        ( [], _ ) ->
            right


type alias Model =
    { time : Float
    , questions : List QuestionType
    , score : Int
    , debug : String
    , question : QuestionType
    , choices : List String
    , answer : String
    , totalcorrect : Int
    , totalincorrect : Int
    , index : Int
    , themeColors : List Color
    }


init : Model
init =
    { time = 0
    , score = 0
    , choices = []
    , answer = ""
    , question = getQuestion 0 qbank
    , questions = qbank
    , debug = ""
    , index = 0
    , totalcorrect = 0
    , totalincorrect = 0
    , themeColors = defaultColors
    }



-- [SelectedTab, Background, Water, LayerWater, Instruction, (-)btns choice, btn, info, font]


defaultColors =
    [ rgb 171 191 221, rgb 239 245 255, rgb 172 191 250, rgb 94 119 211, rgb 14 110 198, rgb 234 160 238, rgb 193 163 249, darkBlue, black ]


getColor index themeColors =
    case List.head <| List.drop index themeColors of
        Just color ->
            color

        Nothing ->
            white


type Msg
    = Tick Float GetKeyState
    | NewQuestion Int
    | NextQuestion String
    | NewIndex Int
    | GetIndex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        model =
            case msg of
                Tick _ _ ->
                    m

                _ ->
                    { m | debug = toString msg }
    in
    case msg of
        Tick t _ ->
            ( { model | time = t }
            , Cmd.none
            )

        NewIndex i ->
            ( { model | index = i }, Cmd.none )

        GetIndex ->
            ( model, getIndex )

        NextQuestion choice ->
            ( { model
                | score =
                    if choice == getAnswer model.question then
                        model.score + 1

                    else
                        model.score - 1
                , totalcorrect =
                    if choice == getAnswer model.question then
                        model.totalcorrect + 1

                    else
                        model.totalcorrect
                , totalincorrect =
                    if choice == getAnswer model.question then
                        model.totalincorrect

                    else
                        model.totalincorrect + 1
                , questions =
                    if choice == getAnswer model.question then
                        model.questions

                    else
                        model.questions ++ [ model.question ]
              }
            , getNewQuestion
            )

        NewQuestion n ->
            ( { model
                | question = getQuestion n model.questions
                , choices =
                    getChoices model.index model.question
                , answer = getAnswer model.question
              }
            , Cmd.none
            )


view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init =
            \_ ->
                ( init
                  -- this is the initial state, like you are used to
                , Cmd.none
                )

        -- this requests the first random number
        , update = update
        , view = \model -> { title = "Random", body = view model }
        , subscriptions = \_ -> Sub.none
        }
