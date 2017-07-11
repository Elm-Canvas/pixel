module Main exposing (..)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Canvas exposing (Size, Point, DrawOp(..), Error, Canvas)
import Canvas.Pixel as Pixel exposing (Position)
import Task
import ElementRelativeMouseEvents as Events
import Color exposing (Color)


-- MAIN --


main =
    Html.program
        { init = ( Loading, loadImage )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- TYPES


type Msg
    = ImageLoaded (Result Error Canvas)
    | Move Position


type Model
    = Loaded Canvas (Maybe ( Position, Color ))
    | Loading


loadImage : Cmd Msg
loadImage =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage "./arizona.jpg")



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model ) of
        ( ImageLoaded (Ok canvas), Loading ) ->
            ( Loaded canvas Nothing, Cmd.none )

        ( Move position, Loaded canvas _ ) ->
            let
                pixel =
                    Just ( position, Pixel.get position canvas )
            in
                ( Loaded canvas pixel
                , Cmd.none
                )

        _ ->
            model ! []



-- VIEW --


view : Model -> Html Msg
view model =
    div
        []
        [ p [] [ text "Mouse over pixels to enlarge them" ]
        , viewIfReady model
        ]


viewIfReady : Model -> Html Msg
viewIfReady model =
    case model of
        Loading ->
            p [] [ text "Loading image" ]

        Loaded canvas Nothing ->
            viewCanvas canvas

        Loaded canvas (Just (position, color)) ->
            let
                rect =
                    Rect
                        (toPoint position)
                        (Size 100 100)
            in
                canvas
                    |> Canvas.draw (patch color rect)
                    |> viewCanvas


viewCanvas : Canvas -> Html Msg
viewCanvas =
    Canvas.toHtml [ Events.onMouseMove (Move << toPosition) ]


patch : Color -> DrawOp -> DrawOp
patch color rect =
    [ BeginPath
    , rect
    , FillStyle color
    , Fill
    , BeginPath
    , rect
    , StrokeStyle Color.red
    , Stroke
    ]
        |> Canvas.batch



-- HELPERS --


toPoint : Position -> Point
toPoint { x, y } =
    Point (toFloat x) (toFloat y)


toPosition : Point -> Position
toPosition { x, y } =
    Position (floor x) (floor y)
