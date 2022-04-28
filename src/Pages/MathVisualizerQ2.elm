{- Dependencies:
   - Pages.MathVisualizerQ2MCQ
   - Pages.GraphV1
-}


module Pages.MathVisualizerQ2 exposing (Model, Msg(..), State(..), arrowWithLabel, building, button, contentPanel, darkColors, defaultColors, funBtnsQ2, getColor, getResultQ2, infomationQ2, init, instructionQ2, lightColors, main, myShapes, pullingCable, relevantFormulasQ2, setLatexColor, update, view)

import Html
import Html.Attributes
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Pages.MathVisualizerQ2MCQ exposing (..)
import Pages.GraphV1 exposing (..)


button model string color l w fontSize =
    group
        [ roundedRect l w 1
            |> filled color
        , text string
            |> sansserif
            |> centered
            |> size fontSize
            |> filled (getColor 8 model.themeColors)
            |> move ( 0, -2 )
        ]


contentPanel model =
    roundedRect 190 114 5
        |> filled (getColor 1 model.themeColors)
        |> addOutline (solid 0.4) black
        |> move ( 0, -6.25 )


building model =
    group
        [ rect 40 60
            |> filled (getColor 2 model.themeColors)
            |> addOutline (solid 0.2) black
            |> move ( 0, 0 )
        , group <|
            List.map
                (\x ->
                    group <|
                        (rect 40 3
                            |> filled (getColor 3 model.themeColors)
                            |> addOutline (solid 0.1) black
                            |> move ( 0, 22 - 5.2 * toFloat x )
                        )
                            :: List.map
                                (\xx ->
                                    line ( -20, 23.5 - 5.2 * toFloat x ) ( -20, 20.5 - 5.2 * toFloat x )
                                        |> outlined (solid 0.1) black
                                        |> move ( toFloat xx * 5, 0 )
                                )
                                (List.range 0 8)
                )
                (List.range 0 8)
        , rect 12 7
            |> filled (getColor 3 model.themeColors)
            |> addOutline (solid 0.1) black
            |> move ( 0, -26.5 )
        , line ( 0, -23 ) ( 0, -30 ) |> outlined (solid 0.1) black
        ]


pullingCable model posY =
    group
        [ clip (line ( 22, 30 ) ( 22, -25 ) |> outlined (solid 0.5) black) (rect 5 60 |> filled (getColor 8 model.themeColors) |> move ( 22, posY )) ]


arrowWithLabel model string length x label =
    if x then
        group
            [ line ( 0, 0 ) ( length, 0 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, 0 ) ( 1, 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, 0 ) ( 1, -1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( length, 0 ) ( length - 1, 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( length, 0 ) ( length - 1, -1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , text string
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( length / 2 - toFloat (String.length string), 5 * label )
            ]

    else
        group
            [ line ( 0, 0 ) ( 0, length )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, 0 ) ( 1, 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, 0 ) ( -1, 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, length ) ( 1, length - 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , line ( 0, length ) ( -1, length - 1 )
                |> outlined (solid 0.2) (getColor 8 model.themeColors)
            , text string
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> (let
                        pos_x =
                            if label < 0 then
                                -8

                            else
                                1 * label
                    in
                    move ( pos_x, length / 2 - toFloat (String.length string) )
                   )
            ]


relevantFormulasQ2 model =
    group
        [ roundedRect 40 (50 + 35) 5
            |> outlined (solid 0.4) black
            |> move ( 70, 8.25 )
        , text "Relevant Formulas"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 52.5, 45 )
        , line ( 50, 43 ) ( 50 + 40, 43 )
            |> outlined (solid 0.4) black
        , text "Gravity"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 64, 38 )
        , html 500
            500
            (Html.img
                [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "G=mg")
                , Html.Attributes.draggable "false"
                , Html.Attributes.style "user-select" "none"
                ]
                []
            )
            |> scale 0.2
            |> move ( 60, 32 )
        , line ( 50, 23 - 5 ) ( 50 + 40, 23 - 5 )
            |> outlined (solid 0.3) black
        , text "Work"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 66, 13 )
        , html 500
            200
            (Html.img
                [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "W=Fd")
                , Html.Attributes.draggable "false"
                , Html.Attributes.style "user-select" "none"
                ]
                []
            )
            |> scale 0.2
            |> move ( 58, 8 )
        , line ( 50, -8 ) ( 50 + 40, -8 )
            |> outlined (solid 0.3) black
        ]
        |> move ( 5, 0 )


infomationQ2 model =
    group
        [ roundedRect 155 25 5
            |> outlined (solid 0.4) black
            |> move ( 20 - 2.5, -50.5 )
        , line ( 0, -38 ) ( 0, -25 - 38 )
            |> outlined (solid 0.4) black
            |> move ( 28, 0 )
        , [ text "Mass (m):"
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -57, -44 - 2.5 )
          , text "Length (l):"
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -57, -60 + 2.5 )
          , button model "+" (getColor 6 model.themeColors) 6 6 6
                |> move ( -5, -42.5 - 2.5 )
                |> notifyTap Q2MP
          , button model "+" (getColor 6 model.themeColors) 6 6 6
                |> move ( -5, -58.5 + 2.5 )
                |> notifyTap Q2LP
          , button model "-" (getColor 5 model.themeColors) 6 6 6
                |> move ( -23, -42.5 - 2.5 )
                |> notifyTap Q2MM
          , button model "-" (getColor 5 model.themeColors) 6 6 6
                |> move ( -23, -58.5 + 2.5 )
                |> notifyTap Q2LM
          , text (String.fromFloat model.q2M)
                |> centered
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -15, -44 - 2.5 )
          , text (String.fromFloat model.q2L)
                |> centered
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -15, -60 + 2.5 )
          ]
            |> group
            |> move ( 90, 0 )
        , [ text "A m kg cable is l m long and hangs vertically from the"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -42 )
          , text "top of a tall building. How much work is required to"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -47 )
          , text "lift the cable to the top of the building?"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -52 )
          , text "(Use g=10 m/s^2)"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -57 )
          ]
            |> group
            |> move ( -58, -2 )
        ]
        |> move ( -35, -0.25 )


instructionQ2 model =
    group
        [ text "First, let us have a look at work at" |> sansserif |> size 5 |> filled (getColor 4 model.themeColors) |> move ( -40, 40 )
        , text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 4 model.themeColors) |> move ( 37, 40 )
        , text "i" |> customFont "Times New Roman" |> size 3 |> italic |> filled (getColor 4 model.themeColors) |> move ( 39, 39.5 )
        , group
            [ html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "W=Fd=mgx_i")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
            , html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "=10(" ++ String.fromFloat model.q2M ++ "/" ++ String.fromFloat model.q2L ++ ")x_i\\Delta x")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
                |> move ( 10, -8 )
            ]
            |> move ( -40, 36 )
        , text "Recall the equation" |> sansserif |> size 5 |> filled (getColor 4 model.themeColors) |> move ( -40, 15 )
        , html 500
            200
            (Html.img
                [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "\\int^b_af(x)\\,dx=\\lim_{n\\to\\infty}\\sum^n_{i=1}f(x_i)\\Delta x")
                , Html.Attributes.draggable "false"
                , Html.Attributes.style "user-select" "none"
                ]
                []
            )
            |> scale 0.2
            |> move ( -35, 13 )
        , text "We get the following integral" |> sansserif |> size 5 |> filled (getColor 4 model.themeColors) |> move ( -40, -10 )
        , group
            [ html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "\\int^{" ++ String.fromFloat model.q2L ++ "}_0" ++ "10(" ++ String.fromFloat model.q2M ++ "/" ++ String.fromFloat model.q2L ++ ")x")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    , Html.Attributes.style "pointer-events" "none"
                    ]
                    []
                )
                |> scale 0.2
            , html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "=" ++ String.fromFloat (getResultQ2 model.q2M model.q2L) ++ "J")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    , Html.Attributes.style "pointer-events" "none"
                    ]
                    []
                )
                |> scale 0.2
                |> move ( 5, -16 )
            ]
            |> move ( -35, -12 )
        ]


getResultQ2 m l =
    5 * (l ^ 2) * (m / l)


funBtnsQ2 model =
    group
        [ button model "Reset Value" (getColor 6 model.themeColors) 33 7.5 4
            |> move ( -77.5, -45 )
            |> notifyTap Q2Reset
        , button model "Similar Question" (getColor 6 model.themeColors) 33 7.5 4
            |> move ( -77.5, -56 )
            |> notifyTap ToQ2Similar
        ]
        |> move ( 155, 0 )


myShapes model =
    case model.state of
        QuestionType2 ->
            [ contentPanel model
            , if model.q2Playing then
                group
                    [ building model
                    , pullingCable model model.q2CablePosY
                    ]
                    |> scale 0.9
                    |> move ( -65, -5 )

              else
                group
                    [ building model
                    , pullingCable model 0
                    , arrowWithLabel model (String.fromFloat model.q2L ++ "m") 55 False 1 |> move ( 21, -25 )
                    , rect 2 3 |> outlined (solid 0.2) (getColor 8 model.themeColors) |> move ( 23, 10 )
                    , group
                        [ text "Δ" |> customFont "Times New Roman" |> size 5 |> filled (getColor 8 model.themeColors) |> move ( -3.5, -7 )
                        , text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( 0, -7 )
                        ]
                        |> move ( 28, 16 )
                    , arrowWithLabel model "" 20 False 1 |> move ( 18, 10 )
                    , group
                        [ text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( -2, -15 )
                        , text "i" |> customFont "Times New Roman" |> size 3 |> italic |> filled (getColor 8 model.themeColors) |> move ( 0, -15.5 )
                        ]
                        |> move ( 16, 39 )
                    ]
                    |> scale 0.9
                    |> move ( -65, -5 )
            , if model.q2ShowGraph then
                group
                    [ html 500
                        200
                        (Html.img
                            [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "10(" ++ String.fromFloat model.q2M ++ "/" ++ String.fromFloat model.q2L ++ ")x")
                            , Html.Attributes.draggable "false"
                            , Html.Attributes.style "user-select" "none"
                            ]
                            []
                        )
                        |> scale 0.15
                        |> move ( -25, 10 )
                    , Pages.GraphV1.q2Graph model.q2M model.q2L (getColor 8 model.themeColors) |> scale 0.7 |> move ( -30, -30 )
                    ]

              else
                instructionQ2 model
            , relevantFormulasQ2 model
            , infomationQ2 model
            , funBtnsQ2 model
            , group
                [ text "Play Animation: " |> sansserif |> size 4 |> filled (getColor 8 model.themeColors) |> move ( -30, 1.5 )
                , text
                    (if model.q2Playing then
                        "⏹️"

                     else
                        "▶️"
                    )
                    |> size 7
                    |> filled (getColor 8 model.themeColors)
                    |> notifyTap Q2Play
                , let
                    dis =
                        if model.q2ShowGraph then
                            "Hide Graph: "

                        else
                            "Show Graph: "
                  in
                  text dis |> sansserif |> size 4 |> filled (getColor 8 model.themeColors) |> move ( -30, -8.5 )
                , text "📈" |> size 7 |> filled (getColor 8 model.themeColors) |> move ( 0, -10 ) |> notifyTap Q2ShowGraph
                ]
                |> move ( -55, 40 )
            ]

        Q2Similar ->
            [ Pages.MathVisualizerQ2MCQ.myShapes model.q2Mcq |> group |> GraphicSVG.map Q2MCQMsg
            , group
                [ building model
                , pullingCable model model.q2CablePosY
                ]
                |> move ( -65, 0 )
            , group
                [ button model "Home" (getColor 6 model.themeColors) 33 7.5 4
                    |> move ( -77.5, -45 )
                    |> notifyTap ToHome
                , button model "Back" (getColor 6 model.themeColors) 33 7.5 4
                    |> move ( -77.5, -56 )
                    |> notifyTap ToQ2
                ]
                |> move ( 155, 0 )
            ]


type Msg
    = Tick Float GetKeyState
    | ToQ2
    | ToHome
    | Q2MCQMsg Pages.MathVisualizerQ2MCQ.Msg
    | Q2Play
    | Q2ShowGraph
    | Q2MP
    | Q2MM
    | Q2LP
    | Q2LM
    | Q2Reset
    | ToQ2Similar


type State
    = QuestionType2
    | Q2Similar


update msg model =
    case msg of
        Tick t _ ->
            ( { model
                | time = t
                , q2CablePosY =
                    if model.q2Playing then
                        model.q2CablePosY + 0.2

                    else
                        0
                , q2Playing =
                    if model.q2Playing && model.q2CablePosY < 60 then
                        True

                    else
                        False
                , q2ShowGraph =
                    if model.q2Playing then
                        True

                    else if model.q2CablePosY > 60 then
                        False

                    else
                        model.q2ShowGraph
              }
            , Cmd.none
            )

        ToQ2 ->
            ( { model | state = QuestionType2 }, Cmd.none )

        ToHome ->
            ( model, Cmd.none )

        Q2MCQMsg mcqMsg ->
            let
                ( newModel, newCmd ) =
                    Pages.MathVisualizerQ2MCQ.update mcqMsg model.q2Mcq
            in
            ( { model | q2Mcq = newModel }
            , Cmd.map Q2MCQMsg newCmd
            )

        Q2Play ->
            ( { model
                | q2Playing = not model.q2Playing
                , q2CablePosY = 0
                , q2ShowGraph =
                    if model.q2Playing then
                        False

                    else
                        True
              }
            , Cmd.none
            )

        Q2ShowGraph ->
            ( { model | q2ShowGraph = not model.q2ShowGraph }, Cmd.none )

        Q2MP ->
            ( { model
                | q2M = model.q2M + 1
              }
            , Cmd.none
            )

        Q2MM ->
            ( { model
                | q2M =
                    if model.q2M == 1 then
                        1

                    else
                        model.q2M - 1
              }
            , Cmd.none
            )

        Q2LP ->
            ( { model
                | q2L = model.q2L + 1
              }
            , Cmd.none
            )

        Q2LM ->
            ( { model
                | q2L =
                    if model.q2L == 1 then
                        1

                    else
                        model.q2L - 1
              }
            , Cmd.none
            )

        Q2Reset ->
            ( { model | q2M = 80, q2L = 160 }, Cmd.none )

        ToQ2Similar ->
            let
                oldQ2Mcq =
                    Pages.MathVisualizerQ2MCQ.init

                newQ2Mcq =
                    { oldQ2Mcq | themeColors = model.themeColors }
            in
            ( { model | state = Q2Similar, q2Mcq = newQ2Mcq }, Cmd.none )


type alias Model =
    { time : Float
    , state : State
    , theme : Int
    , themeColors : List Color
    , q2Mcq : Pages.MathVisualizerQ2MCQ.Model
    , q2M : Float
    , q2L : Float
    , q2Playing : Bool
    , q2CablePosY : Float
    , q2ShowGraph : Bool
    }



-- [SelectedTab, Background, Water, LayerWater, Instruction, (-)btns choice, btn, info, font]


defaultColors =
    [ rgb 171 191 221, rgb 239 245 255, rgb 172 191 250, rgb 94 119 211, rgb 14 110 198, rgb 234 160 238, rgb 193 163 249, darkBlue, black ]


darkColors =
    [ rgb 18 18 18, rgb 60 60 60, rgb 150 150 150, rgb 100 100 100, rgb 145 189 250, rgb 150 150 150, rgb 100 100 100, rgb 145 189 250, rgb 225 225 225 ]


lightColors =
    [ rgb 225 225 225, white, white, white, rgb 94 119 211, rgb 225 225 225, rgb 200 200 200, darkBlue, black ]


getColor index themeColors =
    case List.head <| List.drop index themeColors of
        Just color ->
            color

        Nothing ->
            white


setLatexColor : Model -> String
setLatexColor model =
    case model.theme of
        1 ->
            "\\color[rgb]{.8820,.8820,.8820}"

        _ ->
            "\\color[rgb]{.1,.1,.1}"


init : Model
init =
    { time = 0
    , state = QuestionType2
    , theme = 0
    , themeColors = defaultColors
    , q2Mcq = Pages.MathVisualizerQ2MCQ.init
    , q2M = 80
    , q2L = 160
    , q2Playing = False
    , q2CablePosY = 0
    , q2ShowGraph = False
    }


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init =
            \_ ->
                ( init
                  -- this is the initial state, like you are used to
                , Cmd.none
                )

        -- no requests at this time
        , update = update
        , view = \model -> { title = "Game Slot", body = view model }
        , subscriptions = \_ -> Sub.none
        }


view model =
    collage 192 128 (myShapes model)
