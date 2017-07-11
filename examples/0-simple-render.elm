module Main exposing (..)

import Canvas exposing (Size, Point, Canvas, DrawOp(..))
import Canvas.Pixel as Pixel exposing (Position)
import Html.Attributes exposing (style)
import Color exposing (Color)


main =
    Canvas.toHtml
        [ style
            [ ( "width", "800px" )
            , ( "height", "800px" )
            , ( "image-rendering", "pixelated" )
            ]
        ]
        (draw (Canvas.initialize (Size 5 5)))


draw : Canvas -> Canvas
draw canvas =
    let
        drawOp =
            [ BeginPath
            , Rect
                (Point 0 0)
                (Canvas.getSize canvas)
            , FillStyle Color.blue
            , Fill
            ]
                |> Canvas.batch
    in
        Canvas.draw
            (Canvas.batch [ drawOp, drawPixels ])
            canvas


drawPixels : DrawOp
drawPixels =
    [ Pixel.put Color.red (Position 1 1)
    , Pixel.put Color.red (Position 3 1)
    , Pixel.put Color.red (Position 1 3)
    , Pixel.put Color.red (Position 3 3)
    , Pixel.put Color.red (Position 2 2)
    ]
        |> Canvas.batch
