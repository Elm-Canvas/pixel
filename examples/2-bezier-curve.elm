module Main exposing (..)

import Html exposing (Html, div, input)
import Html.Attributes exposing (style, value, type_)
import Html.Events exposing (onClick)
import Canvas.Pixel as Pixel exposing (Position)
import Canvas exposing (Canvas, DrawOp(..), Point, Size)
import Color


-- MAIN --


main =
    Html.beginnerProgram
        { model = 5
        , view = view
        , update = update
        }



-- UPDATE --


update : Int -> Int -> Int
update delta resolution =
    delta + resolution



-- VIEW --


view : Int -> Html Int
view resolution =
    div
        []
        [ Canvas.toHtml
            [ style
                [ ( "width", "800px" )
                , ( "height", "800px" )
                , ( "image-rendering", "pixelated" )
                ]
            ]
            (Canvas.draw (drawBezier resolution) canvas)
        , input
            [ value "+ 1"
            , onClick 1
            , type_ "submit"
            ]
            []
        , input
            [ value "- 1"
            , onClick -1
            , type_ "submit"
            ]
            []
        ]



-- CANVAS --


canvas : Canvas
canvas =
    let
        size =
            Size 100 100

        drawOp =
            [ BeginPath
            , Rect (Point 0 0) size
            , FillStyle Color.blue
            , Fill
            ]
                |> Canvas.batch
    in
        Canvas.draw drawOp
            (Canvas.initialize size)


drawBezier : Int -> DrawOp
drawBezier resolution =
    Pixel.bezier
        Color.red
        resolution
        (Position 90 90)
        (Position 90 10)
        (Position 10 90)
        (Position 10 10)
