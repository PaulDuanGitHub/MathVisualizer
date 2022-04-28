module Pages.MathVisualizerHome exposing (Model, Msg(..), State(..), contentPanel, darkColors, defaultColors, getColor, init, lightColors, main, myShapes, selectedTab, tab, update, view)

import Html
import Html.Attributes
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

contentPanel model =
    roundedRect 190 114 5
        |> filled (getColor 1 model.themeColors)
        |> addOutline (solid 0.4) black
        |> move ( 0, -6.25 )


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
            [ contentPanel model
            , group
                [ roundedRect 7 7 1 |> filled (getColor 0 model.themeColors)
                , text "ℹ️" |> centered |> size 5 |> filled (getColor 7 model.themeColors) |> move ( 0, -1.5 )
                ]
                |> move ( -85, 42 )
                |> notifyTap ToAbout
            , text "Welcome to the integral application question visualizer!" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, 30 )
            , text "Select a question type above to begin your learning." |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, 20 )
            , line ( -80, 0 ) ( 80, 0 ) |> outlined (solid 0.3) black
            , text "Theme:" |> sansserif |> size 5 |> filled (getColor 8 model.themeColors) |> move ( -80, -7 )
            , roundedRect 30 10 1 |> filled (rgb 171 191 221) |> addOutline (solid 0.3) black |> move ( -60, -20 ) |> notifyTap (ChangeTheme 0)
            , roundedRect 30 10 1 |> filled (rgb 18 18 18) |> addOutline (solid 0.3) black |> move ( 0, -20 ) |> notifyTap (ChangeTheme 1)
            , roundedRect 30 10 1 |> filled white |> addOutline (solid 0.3) black |> move ( 60, -20 ) |> notifyTap (ChangeTheme 2)
            , let
                pos =
                    if model.theme == 2 then
                        ( 60, -22.5 )

                    else if model.theme == 1 then
                        ( 0, -22.5 )

                    else
                        ( -60, -22.5 )

                clr =
                    if model.theme == 2 then
                        black

                    else if model.theme == 1 then
                        white

                    else
                        white
              in
              text "✓" |> size 8 |> centered |> filled clr |> move pos
            , text "Default" |> sansserif |> centered |> size 4 |> filled (getColor 8 model.themeColors) |> move ( -60, -30 )
            , text "Dark Mode" |> sansserif |> centered |> size 4 |> filled (getColor 8 model.themeColors) |> move ( 0, -30 )
            , text "Light Mode" |> sansserif |> centered |> size 4 |> filled (getColor 8 model.themeColors) |> move ( 60, -30 )
            ]

        About ->
            [ contentPanel model
            , group
                [ roundedRect 7 7 1 |> filled (getColor 0 model.themeColors)
                , text "⌂" |> centered |> size 9 |> filled (getColor 7 model.themeColors) |> move ( 0, -2 )
                ]
                |> notifyTap ToHome
                |> move ( -85, 42 )
            , text "This integral visualizer was made by" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, 20 )
            , text "Xianzhao Duan" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, 10 )
            , text "Yujia Wang" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, 0 )
            , text "Paarth Kadakia" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, -10 )
            , text "Sahib Khokhar" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, -20 )
            , text "Aadvit Khanna" |> sansserif |> centered |> size 6 |> filled (getColor 8 model.themeColors) |> move ( 0, -30 )
            ]


type Msg
    = Tick Float GetKeyState
    | ChangeTheme Int
    | ToHome
    | ToAbout


type State
    = Home
    | About


update msg model =
    case msg of
        Tick t _ ->
            ( model, Cmd.none )

        ChangeTheme theme ->
            case theme of
                0 ->
                    ( { model | theme = 0, themeColors = defaultColors }, Cmd.none )

                1 ->
                    ( { model | theme = 1, themeColors = darkColors }, Cmd.none )

                2 ->
                    ( { model | theme = 2, themeColors = lightColors }, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        ToHome ->
            ( { model | state = Home }, Cmd.none )

        ToAbout ->
            ( { model | state = About }, Cmd.none )


type alias Model =
    { time : Float
    , state : State
    , theme : Int
    , themeColors : List Color
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
    , theme = 0
    , themeColors = defaultColors
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
        , view = \model -> { title = "", body = view model }
        , subscriptions = \_ -> Sub.none
        }


view model =
    collage 192 128 (myShapes model)
