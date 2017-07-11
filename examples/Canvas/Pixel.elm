module Canvas.Pixel
    exposing
        ( put
        , get
        , line
        , rectangle
        , bezier
        , ellipse
        , circle
        , Position
        )

{-| The basic methods of the canvas element do not lend themselves well to pixel perfect canvas drawing. This module exposes a number of functions which make doing so easy. By pixel perfect, we mean, drawing with no anti-aliased edges.

# Basics
@docs put, get

# Position Calculation
@docs line, rectangle, bezier, circle, ellipse
-}

import Canvas exposing (Canvas, Size, Point, DrawOp(..))
import Color exposing (Color)
import RasterShapes as Shapes exposing (Position)


type alias Position =
    { x : Int
    , y : Int
    }


toPoint : Position -> Point
toPoint { x, y } =
    Point (toFloat x) (toFloat y)


{-| Give `put` a `Color`, and a `Position`, and you have a `DrawOp` which will set that exact pixel to that exact color.

    putRedPixel : Position -> Canvas -> Canvas
    putRedPixel position =
        Canvas.draw (Pixel.put Color.red position)

-}
put : Color -> Position -> DrawOp
put color position =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        PutImageData
            [ red
            , green
            , blue
            , round (alpha * 255)
            ]
            (Size 1 1)
            (toPoint position)


{-| Also as fundamental to `put`, `get` will give you the color value of a specific pixel in a `Canvas`.

    isBlueAt : Position -> Canvas -> Bool
    isBlueAt position canvas =
        (Pixel.get position canvas) == Color.blue
-}
get : Position -> Canvas -> Color
get position canvas =
    let
        data =
            Canvas.getImageData
                (toPoint position)
                (Size 1 1)
                canvas
    in
        case data of
            r :: g :: b :: a :: [] ->
                Color.rgba r g b ((toFloat a) / 255)

            _ ->
                Color.rgba 0 0 0 0


rectangle : Color -> Size -> Position -> DrawOp
rectangle color size position =
    Shapes.rectangle size position
        |> List.map (put color)
        |> Canvas.batch


circle : Color -> Int -> Position -> DrawOp
circle color diameter position =
    Shapes.circle diameter position
        |> List.map (put color)
        |> Canvas.batch


ellipse : Color -> Size -> Position -> DrawOp
ellipse color size position =
    Shapes.ellipse size position
        |> List.map (put color)
        |> Canvas.batch


bezier : Color -> Int -> Position -> Position -> Position -> Position -> DrawOp
bezier color resolution p0 p1 p2 p3 =
    Shapes.bezier resolution p0 p1 p2 p3
        |> List.map (put color)
        |> Canvas.batch


line : Color -> Position -> Position -> DrawOp
line color p q =
    Shapes.line p q
        |> List.map (put color)
        |> Canvas.batch
