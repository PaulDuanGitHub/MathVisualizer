{- Dependencies:
   - Pages.MathVisualizerHome
   - Pages.MathVisualizerQ1
   - Pages.MathVisualizerQ2
-}
-- Please do not spam on plus and minus button, the server will crash.


module MathVisualizerMain exposing (Model, Msg(..), State(..), darkColors, defaultColors, getColor, init, lightColors, main, myShapes, selectedTab, tab, update, view)

import Html
import Html.Attributes
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Pages.MathVisualizerHome exposing (..)
import Pages.MathVisualizerQ1 exposing (..)
import Pages.MathVisualizerQ2 exposing (..)

-- Normal tab


tab model string =
    group
        [ roundedRect 40 8 0.2
            |> filled (getColor 1 model.themeColors)
        , text string
            |> sansserif
            |> centered
            |> size 4
            |> filled (getColor 8 model.themeColors)
            |> move ( 0, -1 )
        ]
        |> addOutline (solid 0.5) black



-- Selected tab


selectedTab model string =
    group
        [ roundedRect 40 10 0.2
            |> filled (getColor 0 model.themeColors)
        , text string
            |> sansserif
            |> centered
            |> size 4.5
            |> filled (getColor 8 model.themeColors)
            |> move ( 0, -1 )
        ]
        |> addOutline (solid 0.5) black
        |> move ( 0, 1 )


myShapes model =
    case model.state of
        Home ->
            [ tab model "Lifting Weights"
                |> move ( -5 + 5, 55 )
                |> notifyTap ToQ2
            , tab model "Water Pumping"
                |> move ( -40 + 5, 55 )
                |> notifyTap ToQ1
            , selectedTab model "Home"
                |> move ( -75 + 5, 55 )
            , GraphicSVG.map HomeMsg <| group <| Pages.MathVisualizerHome.myShapes model.homeModel
            ]

        QuestionType1 ->
            [ tab model "Lifting Weights"
                |> move ( -5 + 5, 55 )
                |> notifyTap ToQ2
            , tab model "Home"
                |> move ( -75 + 5, 55 )
                |> notifyTap ToHome
            , selectedTab model "Water Pumping"
                |> move ( -40 + 5, 55 )
            , GraphicSVG.map Q1Msg <| group <| Pages.MathVisualizerQ1.myShapes model.q1Model
            ]

        QuestionType2 ->
            [ tab model "Water Pumping"
                |> move ( -40 + 5, 55 )
                |> notifyTap ToQ1
            , tab model "Home"
                |> move ( -75 + 5, 55 )
                |> notifyTap ToHome
            , selectedTab model "Lifting Weights"
                |> move ( -5 + 5, 55 )
            , GraphicSVG.map Q2Msg <| group <| Pages.MathVisualizerQ2.myShapes model.q2Model
            ]


type Msg
    = Tick Float GetKeyState
    | ToQ1
    | ToQ2
    | ToHome
    | HomeMsg Pages.MathVisualizerHome.Msg
    | Q1Msg Pages.MathVisualizerQ1.Msg
    | Q2Msg Pages.MathVisualizerQ2.Msg


type State
    = Home
    | QuestionType1
    | QuestionType2


update msg model =
    case msg of
        Tick t keys ->
            case model.state of
                QuestionType1 ->
                    let
                        ( newModel, newCmd ) =
                            Pages.MathVisualizerQ1.update (Pages.MathVisualizerQ1.Tick t keys) model.q1Model
                    in
                    ( { model | q1Model = newModel }
                    , Cmd.none
                    )

                QuestionType2 ->
                    let
                        ( newModel, newCmd ) =
                            Pages.MathVisualizerQ2.update (Pages.MathVisualizerQ2.Tick t keys) model.q2Model
                    in
                    ( { model | q2Model = newModel }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToQ1 ->
            ( { model
                | state = QuestionType1
                , q1Model =
                    let
                        oldQ1Model =
                            model.q1Model
                    in
                    { oldQ1Model | state = Pages.MathVisualizerQ1.QuestionType1 }
              }
            , Cmd.none
            )

        ToQ2 ->
            ( { model
                | state = QuestionType2
                , q2Model =
                    let
                        oldQ2Model =
                            model.q2Model
                    in
                    { oldQ2Model | state = Pages.MathVisualizerQ2.QuestionType2 }
              }
            , Cmd.none
            )

        ToHome ->
            ( { model | state = Home }, Cmd.none )

        HomeMsg homeMsg ->
            case homeMsg of
                -- Change theme here
                Pages.MathVisualizerHome.ChangeTheme i ->
                    let
                        ( newModel, newCmd ) =
                            -- Home module
                            Pages.MathVisualizerHome.update homeMsg model.homeModel
                    in
                    ( { model
                        | homeModel = newModel
                        , themeColors = newModel.themeColors

                        -- QuestionType1 module
                        , q1Model =
                            let
                                oldQ1Model =
                                    model.q1Model
                            in
                            { oldQ1Model | themeColors = newModel.themeColors, theme = i }

                        -- QuestionType2 module
                        , q2Model =
                            let
                                oldQ2Model =
                                    model.q2Model
                            in
                            { oldQ2Model | themeColors = newModel.themeColors, theme = i }
                      }
                    , Cmd.map HomeMsg newCmd
                    )

                _ ->
                    let
                        ( newModel, newCmd ) =
                            Pages.MathVisualizerHome.update homeMsg model.homeModel
                    in
                    ( { model | homeModel = newModel }
                    , Cmd.map HomeMsg newCmd
                    )

        Q1Msg q1Msg ->
            case q1Msg of
                Pages.MathVisualizerQ1.ToHome ->
                    ( { model | state = Home }, Cmd.none )

                _ ->
                    let
                        ( newModel, newCmd ) =
                            Pages.MathVisualizerQ1.update q1Msg model.q1Model
                    in
                    ( { model | q1Model = newModel }
                    , Cmd.map Q1Msg newCmd
                    )

        Q2Msg q2Msg ->
            case q2Msg of
                Pages.MathVisualizerQ2.ToHome ->
                    ( { model | state = Home }, Cmd.none )

                _ ->
                    let
                        ( newModel, newCmd ) =
                            Pages.MathVisualizerQ2.update q2Msg model.q2Model
                    in
                    ( { model | q2Model = newModel }
                    , Cmd.map Q2Msg newCmd
                    )


type alias Model =
    { time : Float
    , state : State
    , themeColors : List Color
    , homeModel : Pages.MathVisualizerHome.Model
    , q1Model : Pages.MathVisualizerQ1.Model
    , q2Model : Pages.MathVisualizerQ2.Model
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


init : Model
init =
    { time = 0
    , state = Home
    , themeColors = defaultColors
    , homeModel = Pages.MathVisualizerHome.init
    , q1Model = Pages.MathVisualizerQ1.init
    , q2Model = Pages.MathVisualizerQ2.init
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
        , view = \model -> { title = "Integral Application Question Visualizer", body = view model }
        , subscriptions = \_ -> Sub.none
        }


view model =
    collage 192 128 (myShapes model)
