module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Canvas exposing (Canvas, Point, Size)
import Canvas.Pixel as Pixel exposing (Position)
import ElementRelativeMouseEvents as Events
import Color


-- MAIN --


main =
    Html.beginnerProgram
        { model =
            ( Canvas.initialize (Size 50 50)
            , NoClicks
            )
        , view = view
        , update = update
        }



-- TYPES --


type ClickState
    = NoClicks
    | FirstClick Position
    | Moving Position Position


type Msg
    = Click Position
    | Move Position


type alias Model =
    ( Canvas, ClickState )



-- UPDATE --


update : Msg -> Model -> Model
update message ( canvas, clickState ) =
    case ( clickState, message ) of
        ( NoClicks, Click position ) ->
            ( canvas, FirstClick position )

        ( Moving position0 position1, Click _ ) ->
            ( drawLine position0 position1 canvas, NoClicks )

        ( Moving position0 position1, Move position ) ->
            ( canvas, Moving position0 position )

        ( FirstClick position0, Move position ) ->
            ( canvas, Moving position0 position )

        _ ->
            ( canvas, clickState )



-- VIEW --


view : Model -> Html Msg
view model =
    Canvas.toHtml
        [ style
            [ ( "border", "1px solid black" )
            , ( "width", "800px" )
            , ( "height", "800px" )
            , ( "image-rendering", "pixelated" )
            ]
        , Events.onMouseDown (Click << toPosition)
        , Events.onMouseMove (Move << toPosition)
        ]
        (handleClickState model)


handleClickState : Model -> Canvas
handleClickState ( canvas, clickState ) =
    case clickState of
        Moving position0 position1 ->
            drawLine position0 position1 canvas

        _ ->
            canvas



-- DRAWING --


drawLine : Position -> Position -> Canvas -> Canvas
drawLine position0 position1 =
    Pixel.line
        Color.black
        (factor position0)
        (factor position1)
        |> Canvas.draw



-- HELPERS --


factor : Position -> Position
factor { x, y } =
    Position (x // 16) (y // 16)


toPosition : Point -> Position
toPosition { x, y } =
    Position (floor x) (floor y)
