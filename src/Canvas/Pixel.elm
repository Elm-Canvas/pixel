module Canvas.Pixel
    exposing 
        ( put
        , get
        , line
        , rectangle
        , bezier
        , ellipse
        , circle
        )

{-| The basic methods of the canvas element do not lend themselves well to pixel perfect canvas drawing. This module exposes a number of functions which make doing so easy. By pixel perfect, we mean, drawing with no anti-aliased edges.

# Basics
@docs put, get

# Point Calculation
@docs line, rectangle, bezier, circle, ellipse
-}



import Canvas exposing (Canvas, Size, Point, DrawOp(..))
import Color exposing (Color)



{-| Give `put` a `Color`, and a `Point`, and you have a `DrawOp` which will set that exact pixel to that exact color.

    putRedPixel : Point -> Canvas -> Canvas
    putRedPixel point =
        Canvas.draw (Pixel.put Color.red point)

-}
put : Color -> Point -> DrawOp
put color point =
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
            point


{-| Also as fundamental to `put`, `get` will give you the color value of a specific pixel in a `Canvas`.

    isBlueAt : Point -> Canvas -> Bool
    isBlueAt point canvas =
        (Pixel.get point canvas) == Color.blue
-}
get : Point -> Canvas -> Color
get point canvas =
    case Canvas.getImageData point (Size 1 1) canvas of
        r :: g :: b :: a :: [] ->
            Color.rgba r g b ((toFloat a) / 255)

        _ ->
            Color.rgba 0 0 0 0



