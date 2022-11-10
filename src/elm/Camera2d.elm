module Camera2d exposing
    ( Camera2d
    , zoomedAt
    , origin, zoom
    , setOrigin, setZoom, setZoomAtScreenPoint
    , translateBy, translateByScreenVector
    , pointToScene, pointToScreen
    , svgViewBox
    )

{-| A Camera2d maps a 2d scene to a screen.

The camera is defined by an origin point within the scene which is at the
center of the camera, and a zoom ratio mapping scene units onto screen units.

The screen is defined by a rectangle defined in screen space.

The camera and screen together form a mapping from scene units and coordinates
onto screen units and coordinates. Functions are provided to map points in both
directions of this transformation. For example, going from screen to scene lets
you map pointer events into the scene.

Functions are provided for moving the cameras origin point in scene or screen
space, as well as for adjusting the zoom ratio.

A Camera2d is similar to an `ianmackenzie/elm-geometry` `Frame2d` but with the
addition of the zoom ratio.

@docs Camera2d


# Constructors

@docs zoomedAt


# Properties

@docs origin, zoom


# Transformations

@docs setOrigin, setZoom, setZoomAtScreenPoint
@docs translateBy, translateByScreenVector


# Mappings between scene and screen space

@docs pointToScene, pointToScreen
@docs svgViewBox

-}

import Frame2d exposing (Frame2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Rectangle2d exposing (Rectangle2d)
import TypedSvg.Attributes
import TypedSvg.Core
import Vector2d exposing (Vector2d)


{-| A Camera onto a 2d scene, centered on a particular origin point in the scene
and with a zoom ratio.
-}
type Camera2d units screenUnits coordinates
    = Camera2d
        { zoomLevel : Quantity Float (Rate screenUnits units)
        , sceneFrame : Frame2d units coordinates { defines : coordinates }
        }


{-| Creates a camera centered at the origin point with the given zoom ratio.
-}
zoomedAt :
    Point2d units coordinates
    -> Quantity Float (Rate screenUnits units)
    -> Camera2d units screenUnits coordinates
zoomedAt originPoint zoomLevel =
    { zoomLevel = zoomLevel
    , sceneFrame = Frame2d.atPoint originPoint
    }
        |> Camera2d


{-| Gets the camera origin point in scene coordinates.
-}
origin : Camera2d units screenUnits coordinates -> Point2d units coordinates
origin (Camera2d { sceneFrame }) =
    Frame2d.originPoint sceneFrame


{-| Gets the cameras current zoom level.
-}
zoom : Camera2d units screenUnits coordinates -> Quantity Float (Rate screenUnits units)
zoom (Camera2d { zoomLevel }) =
    zoomLevel


{-| Shifts the camera origin point to a new location in scene coordinates.
-}
setOrigin :
    Point2d units coordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setOrigin originPoint (Camera2d camera) =
    { camera | sceneFrame = Frame2d.atPoint originPoint }
        |> Camera2d


{-| Shifts the camera by a vector in screne coordinates.
-}
translateBy :
    Vector2d units coordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
translateBy vec (Camera2d camera) =
    let
        ( x, y ) =
            Vector2d.components vec

        sceneVec =
            Vector2d.xyIn camera.sceneFrame
                x
                y

        sceneFrame =
            Frame2d.translateBy sceneVec camera.sceneFrame
    in
    { camera | sceneFrame = sceneFrame }
        |> Camera2d


{-| Shifts the camera origin point by a vector in screen coordinates.

This can be convenient when working with pointers on the screen.

-}
translateByScreenVector :
    Vector2d screenUnits screenCoordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
translateByScreenVector vec (Camera2d camera) =
    let
        ( x, y ) =
            Vector2d.components vec

        sceneVec =
            Vector2d.xyIn camera.sceneFrame
                (Quantity.at_ camera.zoomLevel x)
                (Quantity.at_ camera.zoomLevel y)

        sceneFrame =
            Frame2d.translateBy sceneVec camera.sceneFrame
    in
    { camera | sceneFrame = sceneFrame }
        |> Camera2d


{-| Adjusts the camera zoom level.

The origin point will not be shifted by this operation. The effect of this on
the screen mapping will be that the zoom is centered on the middle of the
screen, and the origin point remains at the center of the screen.

-}
setZoom :
    Quantity Float (Rate screenUnits units)
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setZoom zoomLevel (Camera2d camera) =
    { camera | zoomLevel = zoomLevel }
        |> Camera2d


{-| Adjusts the camera zoom around a point on the screen. The point within the
scene corresponding to the screen point, will be at the same screen point after
the zoom adjustment.

This is convenient when adjusting the zoom around a pointer position, allowing
a user to zoom in on a particular area under the pointer.

Note that if the screen point does not align with the origin point, then the
origin point will be shifted by this operation.

-}
setZoomAtScreenPoint :
    Quantity Float (Rate screenUnits units)
    -> Point2d screenUnits screenCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setZoomAtScreenPoint zoomLevel screenPoint screen (Camera2d camera) =
    let
        zoomedCamera =
            { camera | zoomLevel = zoomLevel }
                |> Camera2d

        beforeZoomPoint =
            pointToScene (Camera2d camera) screen screenPoint

        afterZoomPoint =
            pointToScene zoomedCamera screen screenPoint

        vector =
            -- A vector that shifts the screen point back to its original location
            Vector2d.from afterZoomPoint beforeZoomPoint
    in
    zoomedCamera
        |> translateBy vector


{-| Maps a point in scene space to a point in screen space.

This can be useful when overlaying something in screen space onto a drawing in scene
space, for example a menu or other user interface construction that is not part of
the drawing itself.

-}
pointToScreen :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point2d sceneUnits sceneCoordinates
    -> Point2d screenUnits screenCoordinates
pointToScreen (Camera2d { sceneFrame, zoomLevel }) screen point =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            Rectangle2d.axes screen

        ( transX, transY ) =
            Point2d.coordinatesIn sceneFrame point
    in
    Point2d.xyIn screenFrame
        (Quantity.at zoomLevel transX)
        (Quantity.at zoomLevel transY)


{-| Maps a point in screen space to a point in scene space.

This can be useful when mapping pointer events onto a drawing, since the pointer
events will be described by their screen coordinates.

-}
pointToScene :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point2d screenUnits screenCoordinates
    -> Point2d sceneUnits sceneCoordinates
pointToScene (Camera2d { sceneFrame, zoomLevel }) screen point =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            Rectangle2d.axes screen

        ( transX, transY ) =
            Point2d.coordinatesIn screenFrame point
    in
    Point2d.xyIn sceneFrame
        (Quantity.at_ zoomLevel transX)
        (Quantity.at_ zoomLevel transY)


{-| Given a camera and a rectangle representing a rectangle where a drawing will be
rendered on screen, provides the correct SVG `viewBox` parameters that will yield
the correctly scaled and translated scene space described by that camera.
-}
svgViewBox :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> TypedSvg.Core.Attribute a
svgViewBox ((Camera2d { zoomLevel }) as camera) screen =
    let
        ( w, h ) =
            Rectangle2d.dimensions screen
                |> Tuple.mapBoth Quantity.unwrap Quantity.unwrap

        { x, y } =
            Rectangle2d.interpolate screen 0 0
                |> pointToScene camera screen
                |> Point2d.unwrap

        scaleFactor =
            zoomLevel |> Quantity.unwrap
    in
    TypedSvg.Attributes.viewBox (round x |> toFloat)
        (round y |> toFloat)
        (round (w / scaleFactor) |> toFloat)
        (round (h / scaleFactor) |> toFloat)
