module Main exposing (Model, Msg, main)

import Array
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Camera2d exposing (Camera2d)
import Color
import Css.Global
import Geometry exposing (BScene, BScreen, RScreen, Scene, Screen, VScreen)
import Geometry.Svg
import GestureEvent exposing (GestureAction(..), GestureEvent(..))
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Lazy
import Html.Styled as HS
import Json.Encode as Encode
import Params
import Pixels exposing (Pixels)
import Point2d
import Pointer
import Ports
import Quantity exposing (Unitless)
import Rectangle2d
import Style
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types
    exposing
        ( Align(..)
        , AnchorAlignment(..)
        , CoordinateSystem(..)
        , MeetOrSlice(..)
        , Opacity(..)
        , Paint(..)
        , Scale(..)
        , ShapeRendering(..)
        , StrokeLinecap(..)
        , StrokeLinejoin(..)
        , TextRendering(..)
        , Transform(..)
        )
import Update2 as U2
import Vector2d


config =
    let
        fontSize =
            30

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , zoomStep = 1.1
    , defaultZoom = 1.0
    , maxZoom = 5
    , minZoom = 0.2
    , defaultSize = Vector2d.unitless 400 400
    , noteFontLevels =
        [ 0.5, 0.75, 1, 1.25, 1.5 ]
            |> Array.fromList
    , defaultNoteFontLevel = 2
    , colors =
        { bg = Color.rgb255 225 225 20
        , border = Color.rgb255 225 225 20
        }
    , containerElementId = "zoomable"
    , minimumNoteSize = Vector2d.unitless 60 60
    , editorId = "sticky-1"
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = SizingWindow
    | Ready DrawingModel


type alias DrawingModel =
    { frame : BScreen
    , zoom : Float
    , fontLevel : Int
    , gestures : Pointer.Model GestureEvent Msg Screen
    , gesturesOnDiv : Pointer.Model GestureEvent Msg Screen
    , gestureCondition : GestureCondition
    , camera : Camera2d Unitless Pixels Scene
    , testBox : BScene
    }


type GestureCondition
    = NoGesture
    | MovingCamera (Camera2d Unitless Pixels Scene)


type Msg
    = WindowSize VScreen
    | OnGestureMsg (Pointer.Msg GestureEvent Screen)
    | OnDivGestureMsg (Pointer.Msg GestureEvent Screen)
    | OnGestureDrag (Pointer.DragArgs Screen) GestureEvent
    | OnGestureDragEnd (Pointer.DragArgs Screen) GestureEvent
    | OnGestureTap (Pointer.PointArgs Screen) GestureEvent
    | OnGestureDoubleTap (Pointer.PointArgs Screen) GestureEvent
    | OnGestureZoom (Pointer.ScaleArgs Screen) GestureEvent


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize coordsToSize |> Sub.map WindowSize
    , case model of
        Ready drawing ->
            Pointer.subscriptions
                { onPointerDown = Ports.onPointerDown
                , onPointerUp = Ports.onPointerUp
                , onPointerMove = Ports.onPointerMove
                , onPointerCancel = Ports.onPointerCancel
                }
                drawing.gestures
                (GestureEvent.gestureDecoder config.containerElementId)

        _ ->
            Sub.none
    ]
        |> Sub.batch



-- Window size conversions


coordsToSize : Int -> Int -> VScreen
coordsToSize x y =
    Vector2d.pixels (toFloat x) (toFloat y)


viewportToSize : Viewport -> VScreen
viewportToSize vport =
    Vector2d.pixels vport.viewport.width vport.viewport.height



-- Update for the Application lifecycle.


switchModel : (a -> Model) -> a -> ( Model, Cmd Msg )
switchModel cons state =
    ( cons state
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case ( model, Debug.log "msg" msg ) of
    case ( model, msg ) of
        ( SizingWindow, WindowSize windowSize ) ->
            let
                docPointerHandlers =
                    { drag = OnGestureDrag
                    , dragEnd = OnGestureDragEnd
                    , click = OnGestureTap
                    , doubleClick = OnGestureDoubleTap
                    }

                docPointerHandler =
                    Pointer.empty
                        |> Pointer.onDrag 0 docPointerHandlers
                        |> Pointer.onClick 0 docPointerHandlers
                        |> Pointer.onDoubleClick 0 docPointerHandlers

                divPointerHandlers =
                    { wheel = OnGestureZoom
                    , pinch = OnGestureZoom
                    }

                divPointerHandler =
                    Pointer.empty
                        |> Pointer.onWheel divPointerHandlers
                        |> Pointer.onPinch divPointerHandlers
            in
            U2.pure
                (Ready
                    { frame = windowSizeToFrame windowSize
                    , zoom = config.defaultZoom
                    , fontLevel = config.defaultNoteFontLevel
                    , gestures =
                        Pointer.init Nothing
                            OnGestureMsg
                            |> Pointer.apply docPointerHandler
                    , gesturesOnDiv =
                        Pointer.init Nothing
                            OnDivGestureMsg
                            |> Pointer.apply divPointerHandler
                    , gestureCondition = NoGesture
                    , camera =
                        Camera2d.zoomedAt
                            Point2d.origin
                            (Quantity.rate (Pixels.float config.defaultZoom)
                                (Quantity.float 1.0)
                            )
                    , testBox =
                        BoundingBox2d.fromExtrema
                            { minX = -40.0 |> Quantity.float
                            , minY = -25.0 |> Quantity.float
                            , maxX = 40.0 |> Quantity.float
                            , maxY = 25.0 |> Quantity.float
                            }
                    }
                )

        ( Ready drawing, _ ) ->
            U2.pure drawing
                |> U2.andThen (updateReady msg)
                |> U2.andMap (switchModel Ready)

        _ ->
            U2.pure model


windowSizeToFrame : VScreen -> BScreen
windowSizeToFrame size =
    BoundingBox2d.from
        (Point2d.pixels 0 0)
        (Point2d.xy (Vector2d.xComponent size) (Vector2d.yComponent size))



-- Update for the Ready state.


updateReady : Msg -> DrawingModel -> ( DrawingModel, Cmd Msg )
updateReady msg drawing =
    case ( drawing.gestureCondition, msg ) of
        ( _, WindowSize windowSize ) ->
            U2.pure { drawing | frame = windowSizeToFrame windowSize }

        ( _, OnGestureMsg gestureMsg ) ->
            U2.pure drawing
                |> U2.andThen (processGesture gestureMsg)

        ( _, OnDivGestureMsg gestureMsg ) ->
            U2.pure drawing
                |> U2.andThen (processDivGesture gestureMsg)

        ( _, OnGestureZoom args _ ) ->
            U2.pure drawing
                |> U2.andThen (adjustZoom args)

        ( NoGesture, OnGestureDrag args (Root _) ) ->
            U2.pure drawing
                |> U2.andThen (moveCamera args drawing.camera)

        ( MovingCamera cameraStart, OnGestureDrag args (Root _) ) ->
            U2.pure drawing
                |> U2.andThen (moveCamera args cameraStart)

        ( _, OnGestureDoubleTap _ (ItemWithId "testBox" ActionSelect _ _) ) ->
            U2.pure drawing

        ( _, OnGestureDragEnd _ _ ) ->
            U2.pure drawing
                |> U2.andThen resetGestureCondition

        _ ->
            U2.pure drawing


processGesture : Pointer.Msg GestureEvent Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
processGesture gestureMsg drawing =
    let
        ( newGesturesModel, gestureCmds ) =
            Pointer.update gestureMsg drawing.gestures
    in
    ( { drawing | gestures = newGesturesModel }
    , gestureCmds
    )


processDivGesture : Pointer.Msg GestureEvent Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
processDivGesture gestureMsg drawing =
    let
        ( newGesturesModel, gestureCmds ) =
            Pointer.update gestureMsg drawing.gesturesOnDiv
    in
    ( { drawing | gesturesOnDiv = newGesturesModel }
    , gestureCmds
    )


adjustZoom : Pointer.ScaleArgs Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
adjustZoom wheelEvent drawing =
    let
        newZoom =
            (drawing.zoom * wheelEvent.scale)
                |> clamp config.minZoom config.maxZoom
    in
    U2.pure
        { drawing
            | zoom = newZoom
            , camera =
                Camera2d.setZoomAtScreenPoint
                    (Quantity.rate
                        (Pixels.float newZoom)
                        (Quantity.float 1.0)
                    )
                    wheelEvent.pos
                    drawing.frame
                    drawing.camera
        }


resetGestureCondition : DrawingModel -> ( DrawingModel, Cmd Msg )
resetGestureCondition model =
    { model | gestureCondition = NoGesture }
        |> U2.pure



-- Camera Control


type alias CameraControl a =
    { a
        | frame : BScreen
        , camera : Camera2d Unitless Pixels Scene
        , gestureCondition : GestureCondition
    }


moveCamera :
    Pointer.DragArgs Screen
    -> Camera2d Unitless Pixels Scene
    -> CameraControl a
    -> ( CameraControl a, Cmd Msg )
moveCamera args cameraStart model =
    { model
        | gestureCondition = MovingCamera cameraStart
        , camera =
            Camera2d.translateByScreenVector
                (Vector2d.from args.pos args.startPos)
                cameraStart
    }
        |> U2.pure



-- Styling


offWhite =
    Color.rgb255 248 240 245



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Drawing Example"
    , body =
        [ Css.Global.global Style.global |> HS.toUnstyled
        , body model
        ]
    }


body : Model -> Html Msg
body model =
    H.div
        []
        [ Html.Lazy.lazy fullBody model
        ]


fullBody : Model -> Html Msg
fullBody model =
    case model of
        Ready drawing ->
            let
                attrs =
                    Pointer.on drawing.gesturesOnDiv (GestureEvent.gestureDecoder config.containerElementId)
                        ++ [ HA.style "width" "100%"
                           , HA.style "height" "100%"
                           , HA.style "overflow" "hidden"
                           ]
            in
            H.div
                attrs
                [ Params.view drawing
                , Svg.svg
                    [ SvgAttr.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
                    , Camera2d.svgViewBox drawing.camera drawing.frame
                    , SvgCore.svgNamespace
                    , SvgAttr.shapeRendering RenderGeometricPrecision
                    ]
                    [ gridPattern drawing
                    , background drawing
                    , testBox drawing
                    ]
                ]

        _ ->
            H.div [] []


gridPattern : DrawingModel -> Svg msg
gridPattern drawing =
    let
        start =
            logBase 10 (1 / drawing.zoom)
                + 2
                |> ceiling

        end =
            start + 2
    in
    gridPatterns start end |> Svg.defs []


gridPatterns : Int -> Int -> List (Svg msg)
gridPatterns pow end =
    gridPatternsInner pow pow end []


gridPatternsInner : Int -> Int -> Int -> List (Svg msg) -> List (Svg msg)
gridPatternsInner start pow end accum =
    let
        exp =
            5

        tenpow =
            exp ^ pow |> toFloat

        isTopGrid =
            pow >= (end - 1)

        isBottomGrid =
            pow == start

        gridName =
            if isTopGrid then
                "grid"

            else
                "grid" ++ (tenpow |> round |> String.fromInt)
    in
    if pow >= end then
        accum

    else
        Svg.pattern
            [ HA.id gridName
            , InPx.width tenpow
            , InPx.height tenpow
            , SvgAttr.patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ Svg.rect
                [ InPx.width tenpow
                , InPx.height tenpow
                , Color.rgb255 176 176 176 |> Paint |> SvgAttr.stroke
                , InPx.strokeWidth (tenpow / 1000)
                , if isBottomGrid then
                    Paint offWhite |> SvgAttr.fill

                  else
                    let
                        innerGridName =
                            "grid" ++ (exp ^ (pow - 1) |> String.fromInt)
                    in
                    Reference innerGridName |> SvgAttr.fill
                ]
                []
            ]
            :: accum
            |> gridPatternsInner start (pow + 1) end


background : DrawingModel -> Svg msg
background { frame } =
    let
        skirtScale =
            100

        ( w, h ) =
            BoundingBox2d.dimensions frame
                |> Tuple.mapBoth Pixels.toFloat Pixels.toFloat

        bgArea =
            Rectangle2d.with
                { x1 = -(skirtScale * w) |> Quantity.float
                , y1 = -(skirtScale * h) |> Quantity.float
                , x2 = (2 * skirtScale) * w |> Quantity.float
                , y2 = (2 * skirtScale) * h |> Quantity.float
                }
    in
    Geometry.Svg.rectangle2d
        [ SvgAttr.fill <| Reference "grid"
        , SvgAttr.fillOpacity <| Opacity 1.0
        , InPx.strokeWidth 0
        ]
        bgArea


testBox : DrawingModel -> Svg msg
testBox model =
    let
        rect =
            Geometry.bboxToRect model.testBox
    in
    Geometry.Svg.rectangle2d
        [ HA.property "data-drawing-id" (Encode.string "testBox")
        , HA.property "data-drawing-action" (Encode.string "select")
        , Color.rgb255 120 190 120 |> Paint |> SvgAttr.fill
        , Opacity 1.0 |> SvgAttr.fillOpacity
        , InPx.strokeWidth 0
        ]
        rect
