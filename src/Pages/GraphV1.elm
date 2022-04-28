module Pages.GraphV1 exposing (arrowWithLabel, generateHL, generateVL, myShapes, q1Graph, q2Graph)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

arrowWithLabel string length x clr =
    if x then
        group
            [ line ( 0, 0 ) ( length, 0 )
                |> outlined (solid 0.5) clr
            , line ( length, 0 ) ( length - 1, 1 )
                |> outlined (solid 0.5) clr
            , line ( length, 0 ) ( length - 1, -1 )
                |> outlined (solid 0.5) clr
            , text string
                |> sansserif
                |> size 6
                |> filled clr
                |> move ( length + 3, -1 )
            ]

    else
        group
            [ line ( 0, 0 ) ( 0, length )
                |> outlined (solid 0.5) clr
            , line ( 0, length ) ( 1, length - 1 )
                |> outlined (solid 0.5) clr
            , line ( 0, length ) ( -1, length - 1 )
                |> outlined (solid 0.5) clr
            , text string
                |> sansserif
                |> size 6
                |> filled clr
                |> move ( -1, length + 3 )
            ]



-- Vertical lines


generateVL : Int -> Shape userMsg
generateVL l =
    line ( 10 * toFloat l, 0 ) ( 10 * toFloat l, 100 )
        |> outlined (solid 0.5) black
        |> makeTransparent 0.5



-- Horizontal lines


generateHL : Int -> Shape userMsg
generateHL l =
    line ( 0, 10 * toFloat l ) ( 100, 10 * toFloat l )
        |> outlined (solid 0.5) black
        |> makeTransparent 0.5


q1Graph r h l clr =
    group
        [ arrowWithLabel "x" 110 True clr
            |> move ( -10, 0 )
        , arrowWithLabel "y" 110 False clr
            |> move ( 0, -10 )
        , group (List.map generateVL (List.range 1 10))
        , group (List.map generateHL (List.range 1 10))
        , let
            int_x =
                (9800 * (r ^ 2) * pi * h) * (100 / ((9800 * (r ^ 2) * pi * h) + ((9800 * (r ^ 2) * pi * h) / 4)))

            int_y =
                h * (100 / (h + (h / 4)))
          in
          group
            [ line ( 0, int_x ) ( int_y, 0 )
                |> outlined (solid 0.5) clr
            , polygon [ ( 0, int_x ), ( 0, 0 ), ( 100 / (h + (h / 4)) * l, 0 ), ( 100 / (h + (h / 4)) * l, 100 / ((9800 * (r ^ 2) * pi * h) + ((9800 * (r ^ 2) * pi * h) / 4)) * r ^ 2 * pi * 9800 * (h - l) ) ]
                |> filled (rgb 212 219 243)
                |> makeTransparent 0.8
            ]
        , text (String.fromFloat (h + (h / 4)))
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 96, -5 )
        , text (String.fromInt (ceiling ((9800 * (r ^ 2) * pi * h) + ((9800 * (r ^ 2) * pi * h) / 4))))
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 5, 95 )
        , text (String.fromFloat h)
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 79, -5 )
        , text (String.fromFloat (9800 * (r ^ 2) * h) ++ "π")
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 4, 80 )
        ]


q2Graph m l clr =
    group
        [ arrowWithLabel "x" 110 True clr
            |> move ( -10, 0 )
        , arrowWithLabel "y" 110 False clr
            |> move ( 0, -10 )
        , group (List.map generateVL (List.range 1 10))
        , group (List.map generateHL (List.range 1 10))
        , let
            int_x =
                0

            int_y =
                0
          in
          group
            [ line ( 0, 0 ) ( 100, 100 )
                |> outlined (solid 0.5) clr
            , polygon [ ( 0, 0 ), ( 100, 0 ), ( 100, 100 ) ]
                |> filled (rgb 212 219 243)
                |> makeTransparent 0.8
            ]
        , text (String.fromFloat l)
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 96, -6 )
        , text (String.fromFloat (l * (m / l) * 10))
            |> sansserif
            |> size 6
            |> filled clr
            |> move ( 5, 95 )
        ]


myShapes model =
    [ q2Graph 80 160 blue
        |> move ( -50, -50 )
        |> scale 1
    ]
update msg model = ( model, Cmd.none )

type Msg
    = Tick Float GetKeyState

type alias Model = {}

init : Model
init = {}

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
        , view = \model -> { title = "GraphV1", body = view model }
        , subscriptions = \_ -> Sub.none
        }

view model =
    collage 192 128 (myShapes model)

