{- Dependencies:
   - Pages.MathVisualizerQ1MCQ
   - Pages.GraphV1
-}


module Pages.MathVisualizerQ1 exposing (Model, Msg(..), State(..), arrowWithLabel, button, contentPanel, darkColors, defaultColors, funBtnsQ1, getColor, getResultQ1, infomationQ1, init, instructionQ1, lightColors, main, myShapes, pumpingWater, relevantFormulasQ1, setLatexColor, update, view, waterTank)

import Html
import Html.Attributes
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Pages.MathVisualizerQ1MCQ exposing (..)
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


waterTank model =
    group
        [ rect 40 50
            |> filled (getColor 2 model.themeColors)
            |> move ( 0, -36 )
            |> addOutline (solid 0.2) black
        , rect 40 60
            |> outlined (solid 0.2) black
            |> move ( 0, -31 )
        , rect 41 1
            |> filled (getColor 1 model.themeColors)
            |> move ( 0, -1 )
        ]


pumpingWater model =
    group
        [ rect 40 model.q1WaterSize
            |> filled (getColor 2 model.themeColors)
            |> move ( 0, model.q1WaterPosY )
            |> addOutline (solid 0.2) (getColor 8 model.themeColors)
        , rect 40 60
            |> outlined (solid 0.2) (getColor 8 model.themeColors)
            |> move ( 0, -31 )
        , rect 41 1
            |> filled (getColor 1 model.themeColors)
            |> move ( 0, -1 )
        ]


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


relevantFormulasQ1 model =
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
        , text "Cylinder Volume"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 56, 38 )
        , html 500
            200
            (Html.img
                [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "V=\\pi r^2h")
                , Html.Attributes.draggable "false"
                , Html.Attributes.style "user-select" "none"
                ]
                []
            )
            |> scale 0.2
            |> move ( 57, 32 )
        , line ( 50, 23 - 5 ) ( 50 + 40, 23 - 5 )
            |> outlined (solid 0.3) black
        , text "Density"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 64, 13 )
        , html 500
            500
            (Html.img
                [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "\\rho=\\frac{m}{v}")
                , Html.Attributes.draggable "false"
                , Html.Attributes.style "user-select" "none"
                ]
                []
            )
            |> scale 0.2
            |> move ( 62, 8 )
        , line ( 50, -8 ) ( 50 + 40, -8 )
            |> outlined (solid 0.3) black
        , text "Work"
            |> sansserif
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 66, -12.5 )
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
            |> move ( 58, -20 )
        ]
        |> move ( 5, 0 )


infomationQ1 model =
    group
        [ roundedRect 155 25 5
            |> outlined (solid 0.4) black
            |> move ( 20 - 2.5, -50.5 )
        , line ( 0, -38 ) ( 0, -25 - 38 )
            |> outlined (solid 0.4) black
            |> move ( 28, 0 )
        , [ text "Radius (r):"
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -57, -44 )
          , text "Height (h):"
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -57, -52 )
          , text "Water Level (l):"
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -57, -60 )
          , button model "+" (getColor 6 model.themeColors) 6 6 6
                |> move ( -5, -42.5 )
                |> notifyTap Q1RP
          , button model "+" (getColor 6 model.themeColors) 6 6 6
                |> move ( -5, -50.5 )
                |> notifyTap Q1HP
          , button model "+" (getColor 6 model.themeColors) 6 6 6
                |> move ( -5, -58.5 )
                |> notifyTap Q1LP
          , button model "-" (getColor 5 model.themeColors) 6 6 6
                |> move ( -23, -42.5 )
                |> notifyTap Q1RM
          , button model "-" (getColor 5 model.themeColors) 6 6 6
                |> move ( -23, -50.5 )
                |> notifyTap Q1HM
          , button model "-" (getColor 5 model.themeColors) 6 6 6
                |> move ( -23, -58.5 )
                |> notifyTap Q1LM
          , text (String.fromFloat model.q1R)
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -15, -44 )
          , text (String.fromFloat model.q1H)
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -15, -52 )
          , text (String.fromFloat model.q1L)
                |> sansserif
                |> size 4
                |> filled (getColor 8 model.themeColors)
                |> move ( -15, -60 )
          ]
            |> group
            |> move ( 90, 0 )
        , [ text "A tank has the shape of a cylinder with height h and"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -42 )
          , text "base radius r. It is filled with water to a height of l."
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -47 )
          , text "Find the work required to empty the tank by pumping"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -52 )
          , text "all of the water to the top of the tank."
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -57 )
          , text "(Density of water 1000kg/m^3, g=9.8 m/s^2)"
                |> sansserif
                |> size 3.5
                |> filled (getColor 8 model.themeColors)
                |> move ( 1, -61.5 )
          ]
            |> group
            |> move ( -58, 0 )
        ]
        |> move ( -35, -0.25 )


instructionQ1 model =
    group
        [ text "First, let us have a look at work at" |> sansserif |> size 5 |> filled (getColor 4 model.themeColors) |> move ( -40, 40 )
        , text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 4 model.themeColors) |> move ( 37, 40 )
        , text "i" |> customFont "Times New Roman" |> size 3 |> italic |> filled (getColor 4 model.themeColors) |> move ( 39, 39.5 )
        , group
            [ html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "W=Fd=mg(" ++ String.fromFloat model.q1H ++ "-x_i)")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
            , html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "=1000(\\pi" ++ String.fromFloat model.q1R ++ "^2\\Delta x)9.8(" ++ String.fromFloat model.q1H ++ "-x_i)")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
                |> move ( 8.5, -8 )
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
            |> move ( -40, 13 )
        , text "We get the following integral" |> sansserif |> size 5 |> filled (getColor 4 model.themeColors) |> move ( -40, -10 )
        , group
            [ html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "\\int^{" ++ String.fromFloat model.q1L ++ "}_0" ++ String.fromFloat model.q1R ++ "^2\\pi(" ++ String.fromFloat model.q1H ++ "-x)(1000)(9.8)")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
            , html 500
                200
                (Html.img
                    [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "=" ++ String.fromFloat (getResultQ1 model.q1R model.q1H model.q1L) ++ "\\pi J")
                    , Html.Attributes.draggable "false"
                    , Html.Attributes.style "user-select" "none"
                    ]
                    []
                )
                |> scale 0.2
                |> move ( 5, -16 )
            ]
            |> move ( -40, -12 )
        ]


getResultQ1 r h l =
    9800 * r ^ 2 * h * l - 9800 * r ^ 2 * 0.5 * l ^ 2


funBtnsQ1 model =
    group
        [ button model "Reset Value" (getColor 6 model.themeColors) 33 7.5 4
            |> move ( -77.5, -45 )
            |> notifyTap Q1Reset
        , button model "Similar Question" (getColor 6 model.themeColors) 33 7.5 4
            |> move ( -77.5, -56 )
            |> notifyTap ToQ1Similar
        ]
        |> move ( 155, 0 )


myShapes model =
    case model.state of
        QuestionType1 ->
            [ contentPanel model
            , if model.q1Playing then
                group
                    [ pumpingWater model
                        |> scale 0.9
                        |> move ( -60, 35 )
                    ]
                    |> move ( -5, -10 )

              else
                group
                    [ group
                        [ waterTank model
                        , rect 40 3
                            |> filled (getColor 3 model.themeColors)
                            |> addOutline (solid 0.2) black
                            |> move ( 0, -45 )
                        ]
                        |> scale 0.9
                        |> move ( -60, 35 )
                    , arrowWithLabel model (String.fromFloat model.q1R ++ "m") 18 True -1
                        |> move ( -60, -22 )
                    , arrowWithLabel model (String.fromFloat model.q1H ++ "m") 53 False -1
                        |> move ( -44, -20 )
                    , arrowWithLabel model (String.fromFloat model.q1L ++ "m") 45 False -1
                        |> move ( -80, -20 )
                    , text "Δ" |> customFont "Times New Roman" |> size 5 |> filled (getColor 8 model.themeColors) |> move ( -41.5, -7 )
                    , text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( -38, -7 )
                    , arrowWithLabel model "" 37 False -1
                        |> move ( -73, -4 )
                    , text "-x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( -72, 13 )
                    , text "i" |> customFont "Times New Roman" |> size 3 |> italic |> filled (getColor 8 model.themeColors) |> move ( -68, 12.5 )
                    , text ("+ " ++ String.fromFloat model.q1H) |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( -66, 12.5 )
                    , arrowWithLabel model "" 16 False -1
                        |> move ( -73, -20 )
                    , text "x" |> customFont "Times New Roman" |> size 5 |> italic |> filled (getColor 8 model.themeColors) |> move ( -72, -15 )
                    , text "i" |> customFont "Times New Roman" |> size 3 |> italic |> filled (getColor 8 model.themeColors) |> move ( -70, -15.5 )
                    ]
                    |> move ( -5, -10 )
            , if model.q1ShowGraph then
                group
                    [ html 500
                        200
                        (Html.img
                            [ Html.Attributes.src ("https://www.1xd3latex2svg.tk//getSvg?latex=" ++ setLatexColor model ++ "" ++ String.fromFloat model.q1R ++ "^2\\pi(" ++ String.fromFloat model.q1H ++ "-x)(1000)(9.8)")
                            , Html.Attributes.draggable "false"
                            , Html.Attributes.style "user-select" "none"
                            ]
                            []
                        )
                        |> scale 0.15
                        |> move ( 1, 0 )
                    , Pages.GraphV1.q1Graph model.q1R model.q1H model.q1L (getColor 8 model.themeColors) |> scale 0.7 |> move ( -30, -30 )
                    ]

              else
                instructionQ1 model
            , relevantFormulasQ1 model
            , infomationQ1 model
            , funBtnsQ1 model
            , group
                [ text "Play Animation: " |> sansserif |> size 4 |> filled (getColor 8 model.themeColors) |> move ( -30, 1.5 )
                , text
                    (if model.q1Playing then
                        "⏹️"

                     else
                        "▶️"
                    )
                    |> size 7
                    |> filled (getColor 8 model.themeColors)
                    |> notifyTap Q1Play
                , let
                    dis =
                        if model.q1ShowGraph then
                            "Hide Graph: "

                        else
                            "Show Graph: "
                  in
                  text dis |> sansserif |> size 4 |> filled (getColor 8 model.themeColors) |> move ( -30, -8.5 )
                , text "📈" |> size 7 |> filled (getColor 8 model.themeColors) |> move ( 0, -10 ) |> notifyTap Q1ShowGraph
                ]
                |> move ( -55, 40 )
            ]

        Q1Similar ->
            [ Pages.MathVisualizerQ1MCQ.myShapes model.q1Mcq |> group |> GraphicSVG.map Q1MCQMsg
            , waterTank model
                |> scale 1.0
                |> move ( -60, 35 )
            , group
                [ button model "Home" (getColor 6 model.themeColors) 33 7.5 4
                    |> move ( -77.5, -45 )
                    |> notifyTap ToHome
                , button model "Back" (getColor 6 model.themeColors) 33 7.5 4
                    |> move ( -77.5, -56 )
                    |> notifyTap ToQ1
                ]
                |> move ( 155, 0 )
            ]


type Msg
    = Tick Float GetKeyState
    | ToQ1
    | ToHome
    | Q1RP
    | Q1RM
    | Q1HP
    | Q1HM
    | Q1LP
    | Q1LM
    | Q1Reset
    | ToQ1Similar
    | Q1MCQMsg Pages.MathVisualizerQ1MCQ.Msg
    | Q1ShowGraph
    | Q1Play


type State
    = QuestionType1
    | Q1Similar


update msg model =
    case msg of
        Tick t _ ->
            ( { model
                | time = t
                , q1WaterPosY =
                    if model.q1Playing then
                        model.q1WaterPosY + 0.1

                    else
                        -36
                , q1WaterSize =
                    if model.q1Playing && model.q1WaterPosY > -26 then
                        model.q1WaterSize - 0.2

                    else
                        50
                , q1Playing =
                    if model.q1Playing && model.q1WaterSize > 0 then
                        True

                    else
                        False
                , q1ShowGraph =
                    if model.q1Playing then
                        True

                    else if model.q1WaterSize < 0 then
                        False

                    else
                        model.q1ShowGraph
              }
            , Cmd.none
            )

        ToQ1 ->
            ( { model | state = QuestionType1 }, Cmd.none )

        ToHome ->
            ( model, Cmd.none )

        Q1RP ->
            ( { model | q1R = model.q1R + 1 }, Cmd.none )

        Q1RM ->
            ( { model
                | q1R =
                    if model.q1R == 1 then
                        1

                    else
                        model.q1R - 1
              }
            , Cmd.none
            )

        Q1HP ->
            ( { model | q1H = model.q1H + 1 }, Cmd.none )

        Q1HM ->
            ( { model
                | q1H =
                    if model.q1H == (model.q1L + 1) then
                        model.q1H

                    else
                        model.q1H - 1
              }
            , Cmd.none
            )

        Q1LP ->
            ( { model
                | q1L =
                    if model.q1L == (model.q1H - 1) then
                        model.q1L

                    else
                        model.q1L + 1
              }
            , Cmd.none
            )

        Q1LM ->
            ( { model
                | q1L =
                    if model.q1L == 1 then
                        1

                    else
                        model.q1L - 1
              }
            , Cmd.none
            )

        Q1Reset ->
            ( { model | q1L = 7, q1R = 3, q1H = 8 }, Cmd.none )

        ToQ1Similar ->
            let
                oldQ1Mcq =
                    Pages.MathVisualizerQ1MCQ.init

                newQ1Mcq =
                    { oldQ1Mcq | themeColors = model.themeColors }
            in
            ( { model | state = Q1Similar, q1Mcq = newQ1Mcq }, Cmd.none )

        Q1MCQMsg mcqMsg ->
            let
                ( newModel, newCmd ) =
                    Pages.MathVisualizerQ1MCQ.update mcqMsg model.q1Mcq
            in
            ( { model | q1Mcq = newModel }
            , Cmd.map Q1MCQMsg newCmd
            )

        Q1ShowGraph ->
            ( { model | q1ShowGraph = not model.q1ShowGraph }, Cmd.none )

        Q1Play ->
            ( { model
                | q1Playing = not model.q1Playing
                , q1WaterPosY = -36
                , q1WaterSize = 50
                , q1ShowGraph =
                    if model.q1Playing then
                        False

                    else
                        True
              }
            , Cmd.none
            )


type alias Model =
    { time : Float
    , state : State
    , theme : Int
    , themeColors : List Color
    , q1R : Float
    , q1H : Float
    , q1L : Float
    , q1Mcq : Pages.MathVisualizerQ1MCQ.Model
    , q1ShowGraph : Bool
    , q1Playing : Bool
    , q1StartTime : Float
    , q1WaterPosY : Float
    , q1WaterSize : Float
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
    , state = QuestionType1
    , theme = 0
    , themeColors = defaultColors
    , q1R = 3
    , q1H = 8
    , q1L = 7
    , q1Mcq = Pages.MathVisualizerQ1MCQ.init
    , q1ShowGraph = False
    , q1Playing = False
    , q1StartTime = 0
    , q1WaterPosY = -36
    , q1WaterSize = 50
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
