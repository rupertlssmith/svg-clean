module Pointer exposing
    ( defaultConfig
    , Model, Msg, init, update
    , PointerPorts, subscriptions
    , on
    , DragArgs, PointArgs, ScaleArgs
    , apply, empty
    , onClick, onClickAndHold, onDoubleClick, onDrag, onDragStart
    , onWheel, onPinch
    , onMove, onPointerDown, onPointerUp
    )

{-| Pointer is a high-level pointer API that unifies pointer events over all
pointer types where possible. Pointer types can include mouse, touch, stylus
and so on.

This Pointer module always uses `elm-geometry` primitives to describe pointer
events with `Pixels` for units.


# Configurable parameters.

@docs Config, defaultConfig


# The TEA structure for hooking up the internal state into your application.

@docs Model, Msg, init, update, defaultConfig


# Pointer subscriptions, needed for listening at the overall HTML Document level.

@docs PointerPorts, subscriptions


# Event handlers for adding to the view, for listening to pointer events below the Document level.

@docs on


# Pointer event records.

@docs DragArgs, PointArgs, ScaleArgs


# A DSL for defining use gesture handling behaviour.

@docs apply, empty
@docs onClick, onClickAndHold, onDoubleClick, onDrag, onDragStart
@docs onWheel, onPinch
@docs onMove, onPointerDown, onPointerUp

-}

import Browser.Events
import Dict exposing (Dict)
import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DE
import Maybe.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Task.Extra
import Update2 as U2


type alias Config =
    { dragThreshold : Int
    , holdTimeMillis : Int
    , mouseWheelZoomStep : Float
    }


defaultConfig =
    { dragThreshold = 3
    , holdTimeMillis = 1000
    , mouseWheelZoomStep = 1.1
    }



-- Model


type Model a msg coordinates
    = Model
        { state : State a coordinates
        , buttonHandlers : Dict Int (ButtonHandler a msg coordinates)
        , wheelHandler : Maybe (ScaleArgs coordinates -> a -> msg)
        , pinchHandler : Maybe (ScaleArgs coordinates -> a -> msg)
        , moveHandler : Maybe (Point2d Pixels coordinates -> a -> msg)
        , toMsg : Msg a coordinates -> msg
        , config : Config
        }


type alias State a coordinates =
    { pointers : Dict Int (PointerState a coordinates)
    , gesture : GestureState
    }


type GestureState
    = NoPointer
    | OnePointer
    | TwoPointer


type alias PointerState a coordinates =
    { startPos : Point2d Pixels coordinates
    , lastPos : Point2d Pixels coordinates
    , initialValue : a
    , dragging : Bool
    , button : Int
    }


type alias ButtonHandler a msg coordinates =
    { click : Maybe (PointArgs coordinates -> a -> msg)
    , doubleClick : Maybe (PointArgs coordinates -> a -> msg)
    , clickAndHold : Maybe (PointArgs coordinates -> a -> msg)
    , drag : Maybe (DragArgs coordinates -> a -> msg)
    , dragStart : Maybe (DragArgs coordinates -> a -> msg)
    , dragEnd : Maybe (DragArgs coordinates -> a -> msg)
    , move : Maybe (PointArgs coordinates -> a -> msg)
    , pointerUp : Maybe (PointArgs coordinates -> a -> msg)
    , pointerDown : Maybe (PointArgs coordinates -> a -> msg)
    }


type alias PointArgs coordinates =
    { button : Int
    , pos : Point2d Pixels coordinates
    }


type alias DragArgs coordinates =
    { startPos : Point2d Pixels coordinates
    , pos : Point2d Pixels coordinates
    }


type alias ScaleArgs coordinates =
    { pos : Point2d Pixels coordinates
    , scale : Float
    }



-- Internal Events


type Msg a coordinates
    = PointerUp a (PointerEvent coordinates)
    | PointerDown a (PointerEvent coordinates)
    | PointerClick a (PointerEvent coordinates)
    | PointerMove a (PointerEvent coordinates)
    | PointerCancel
    | MouseWheel a (WheelEvent coordinates)
    | Error Decode.Error


type alias PointerEvent coordinates =
    { pointerId : Int
    , button : Int
    , pos : Point2d Pixels coordinates
    , times : Int
    }


type alias WheelEvent coordinates =
    { pos : Point2d Pixels coordinates
    , deltaY : Float
    , deltaMode : Int
    }



-- TEA Functions


init : Maybe Config -> (Msg a coordinates -> msg) -> Model a msg coordinates
init maybeConfig toMsg =
    { state =
        { pointers = Dict.empty
        , gesture = NoPointer
        }
    , buttonHandlers = Dict.empty
    , wheelHandler = Nothing
    , pinchHandler = Nothing
    , moveHandler = Nothing
    , toMsg = toMsg
    , config = maybeConfig |> Maybe.withDefault defaultConfig
    }
        |> Model


update : Msg a coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
update msg model =
    case msg of
        PointerDown val args ->
            U2.pure model
                |> U2.andThen (initPointerState val args)
                |> U2.andThen (notifyPointerDown val args)

        PointerUp val args ->
            U2.pure model
                |> U2.andThen (checkForDragEnd val args)
                |> U2.andThen (notifyPointerUp val args)
                |> U2.andThen (clearPointerState val args)

        PointerClick val args ->
            U2.pure model
                |> U2.andThen (checkForClickOrDouble val args)

        PointerMove val args ->
            U2.pure model
                |> U2.andThen (notifyPointerMove val args)
                |> U2.andThen (checkForMovementGesture val args)
                |> U2.andThen (recordLastPointerPos args)

        PointerCancel ->
            U2.pure model
                |> U2.andThen clearAllPointerState

        MouseWheel val args ->
            U2.pure model
                |> U2.andThen (scaleWheel val args)

        _ ->
            U2.pure model



-- Pointer subscriptions, needed for listening at the overall HTML Document level.


type alias PointerPorts msg =
    { onPointerDown : (Value -> msg) -> Sub msg
    , onPointerUp : (Value -> msg) -> Sub msg
    , onPointerMove : (Value -> msg) -> Sub msg
    , onPointerCancel : (Value -> msg) -> Sub msg
    }


subscriptions : PointerPorts msg -> Model a msg coordinates -> Decoder a -> Sub msg
subscriptions ports (Model model) decoder =
    let
        portDecoder eventDecoder val =
            case Decode.decodeValue eventDecoder val of
                Ok msg ->
                    msg |> model.toMsg

                Err err ->
                    Error err |> model.toMsg

        down =
            if atLeastOnePointerDownHandler (Model model) then
                ports.onPointerDown
                    (withPointerEvent decoder PointerDown |> portDecoder)

            else
                Sub.none

        up =
            if atLeastOnePointerUpHandler (Model model) then
                ports.onPointerUp
                    (withPointerEvent decoder PointerUp |> portDecoder)

            else
                Sub.none

        move =
            if
                (List.length (activeButtonsWithHandlers (Model model)) > 0)
                    || Maybe.Extra.isJust model.moveHandler
                    || Maybe.Extra.isJust model.pinchHandler
            then
                ports.onPointerMove
                    (withPointerEvent decoder PointerMove |> portDecoder)

            else
                Sub.none

        click =
            if atLeastOneClickHandler (Model model) then
                Browser.Events.onClick
                    (withPointerEvent decoder PointerClick |> Decode.map model.toMsg)

            else
                Sub.none

        cancel =
            ports.onPointerCancel (always PointerCancel >> model.toMsg)
    in
    [ down
    , up
    , move
    , click
    , cancel
    ]
        |> Sub.batch



-- Event handlers for adding to the view, for listening to pointer events below the Document level.


on : Model a msg coordinates -> Decoder a -> List (Html.Attribute msg)
on (Model model) decoder =
    let
        down =
            if
                atLeastOnePointerDownHandler (Model model)
                    || Maybe.Extra.isJust model.pinchHandler
            then
                Html.Events.on "pointerdown"
                    (withPointerEvent decoder PointerDown |> Decode.map model.toMsg)
                    |> Just

            else
                Nothing

        up =
            if
                atLeastOnePointerUpHandler (Model model)
                    || Maybe.Extra.isJust model.pinchHandler
            then
                Html.Events.on "pointerup"
                    (withPointerEvent decoder PointerUp |> Decode.map model.toMsg)
                    |> Just

            else
                Nothing

        move =
            if
                atLeastOneMoveHandler (Model model)
                    || Maybe.Extra.isJust model.pinchHandler
            then
                Html.Events.on "pointermove"
                    (withPointerEvent decoder PointerMove |> Decode.map model.toMsg)
                    |> Just

            else
                Nothing

        click =
            if atLeastOneClickHandler (Model model) then
                Html.Events.on "click"
                    (withPointerEvent decoder PointerClick |> Decode.map model.toMsg)
                    |> Just

            else
                Nothing

        wheel =
            model.wheelHandler
                |> Maybe.map
                    (\_ ->
                        Html.Events.on "wheel"
                            (withWheel decoder MouseWheel |> Decode.map model.toMsg)
                    )

        cancel =
            Html.Events.on "pointercancel" (Decode.succeed PointerCancel |> Decode.map model.toMsg)
                |> Just
    in
    [ down
    , up
    , move
    , click
    , wheel
    , cancel
    ]
        |> List.filterMap identity



-- Control Logic


{-| Intiialize the button state with the start position that it was pressed,
the custom decoder value at that position, and its dragging state cleared.
-}
initPointerState : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
initPointerState value args (Model model) =
    let
        state =
            model.state

        numActivePointersBefore =
            Dict.size model.state.pointers

        pointers =
            Dict.insert
                args.pointerId
                { startPos = args.pos
                , lastPos = args.pos
                , initialValue = value
                , dragging = False
                , button = args.button
                }
                state.pointers

        numActivePointersAfter =
            Dict.size pointers

        newState =
            case ( numActivePointersBefore, numActivePointersAfter ) of
                ( 0, 1 ) ->
                    { state | pointers = pointers, gesture = OnePointer }

                ( 1, 1 ) ->
                    { state | pointers = pointers, gesture = OnePointer }

                ( 1, 2 ) ->
                    { state | pointers = pointers, gesture = TwoPointer }

                ( 2, 2 ) ->
                    { state | pointers = pointers, gesture = TwoPointer }

                ( _, _ ) ->
                    { state | pointers = Dict.empty, gesture = NoPointer }
    in
    { model | state = newState }
        |> Model
        |> U2.pure


{-| Clears the state assocaited with a button.
-}
clearPointerState : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
clearPointerState value args (Model model) =
    let
        state =
            model.state

        numActivePointersBefore =
            Dict.size model.state.pointers

        pointers =
            Dict.remove args.pointerId state.pointers

        numActivePointersAfter =
            Dict.size pointers

        newState =
            case ( numActivePointersBefore, numActivePointersAfter ) of
                ( 0, 1 ) ->
                    { state | pointers = pointers, gesture = OnePointer }

                ( 1, 1 ) ->
                    { state | pointers = pointers, gesture = OnePointer }

                ( 1, 2 ) ->
                    { state | pointers = pointers, gesture = TwoPointer }

                ( 2, 2 ) ->
                    { state | pointers = pointers, gesture = TwoPointer }

                ( _, _ ) ->
                    { state | pointers = Dict.empty, gesture = NoPointer }
    in
    { model | state = newState }
        |> Model
        |> U2.pure


{-| Used when a button is released, to check if that button is currently dragging
and so a drag end event should be generated for it.
-}
checkForDragEnd : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
checkForDragEnd val args (Model model) =
    let
        dragCmd =
            case ( Dict.get args.button model.buttonHandlers, Dict.get args.pointerId model.state.pointers ) of
                ( Just { dragEnd }, Just pointerState ) ->
                    case ( dragEnd, pointerState.dragging ) of
                        ( Just dragEndHandler, True ) ->
                            dragEndHandler
                                { startPos = pointerState.startPos, pos = args.pos }
                                pointerState.initialValue
                                |> Task.Extra.message

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    ( model |> Model, dragCmd )


notifyPointerDown : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
notifyPointerDown val args (Model model) =
    let
        dragCmd =
            case Dict.get args.button model.buttonHandlers of
                Just { pointerDown } ->
                    case pointerDown of
                        Just posHandler ->
                            posHandler
                                { pos = args.pos, button = args.button }
                                val
                                |> Task.Extra.message

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    ( model |> Model, dragCmd )


notifyPointerUp : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
notifyPointerUp val args (Model model) =
    let
        dragCmd =
            case Dict.get args.button model.buttonHandlers of
                Just { pointerUp } ->
                    case pointerUp of
                        Just posHandler ->
                            posHandler
                                { pos = args.pos, button = args.button }
                                val
                                |> Task.Extra.message

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    ( model |> Model, dragCmd )


notifyPointerMove : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
notifyPointerMove val pointerEvent (Model model) =
    let
        moveCmd =
            model.moveHandler
                |> Maybe.map (\handler -> handler pointerEvent.pos val |> Task.Extra.message)
                |> Maybe.withDefault Cmd.none
    in
    ( model |> Model, moveCmd )


{-| Used on pointer movement, to check what gestures can be derived from the movement.

This is split into movement on a single pointer, such as a drag, and movement on multiple
pointers, such as a pinch.

-}
checkForMovementGesture : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
checkForMovementGesture val pointerEvent (Model model) =
    let
        numActivePointers =
            Dict.size model.state.pointers
    in
    case numActivePointers of
        1 ->
            checkForDrag val pointerEvent (Model model)

        2 ->
            checkForPinch val pointerEvent (Model model)

        _ ->
            U2.pure (Model model)


{-| Check a button has been held for more than the drag threshold, in which
case they should be set to dragging, and drag events generated for them.
-}
checkForDrag : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
checkForDrag val pointerEvent (Model model) =
    let
        state =
            model.state

        ( pointerStates, dragMsgs ) =
            Dict.foldl
                (\pointerId pointerState ( stateAccum, msgs ) ->
                    if pointerState.dragging then
                        ( Dict.insert pointerId pointerState stateAccum
                        , addDragMsgIfHandlerExists False pointerState.button pointerState msgs
                        )

                    else if (distance pointerState.startPos pointerEvent.pos |> floor) > model.config.dragThreshold then
                        ( Dict.insert pointerId { pointerState | dragging = True } stateAccum
                        , addDragMsgIfHandlerExists True pointerState.button pointerState msgs
                        )

                    else
                        ( Dict.insert pointerId pointerState stateAccum, msgs )
                )
                ( Dict.empty, [] )
                state.pointers

        addDragMsgIfHandlerExists isStart pointerId pointerState msgs =
            case ( isStart, Dict.get pointerId model.buttonHandlers ) of
                ( _, Nothing ) ->
                    msgs

                ( False, Just { drag } ) ->
                    case drag of
                        Nothing ->
                            msgs

                        Just dragHandler ->
                            dragHandler
                                { startPos = pointerState.startPos, pos = pointerEvent.pos }
                                pointerState.initialValue
                                :: msgs

                ( True, Just { drag, dragStart } ) ->
                    ([ Maybe.map
                        (\dragHandler ->
                            dragHandler
                                { startPos = pointerState.startPos, pos = pointerEvent.pos }
                                pointerState.initialValue
                        )
                        drag
                     , Maybe.map
                        (\dragStartHandler ->
                            dragStartHandler
                                { startPos = pointerState.startPos, pos = pointerEvent.pos }
                                pointerState.initialValue
                        )
                        dragStart
                     ]
                        |> List.filterMap identity
                    )
                        ++ msgs

        newState =
            { state | pointers = pointerStates }

        dragCmds =
            List.map Task.Extra.message dragMsgs
                |> Cmd.batch
    in
    ( { model | state = newState } |> Model, dragCmds )


{-| Check if 2 pointers are moving towards or away from each other.
-}
checkForPinch : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
checkForPinch val pointerEvent (Model model) =
    let
        state =
            model.state
    in
    case Dict.get pointerEvent.pointerId state.pointers of
        Just movedPointerState ->
            let
                maybeStaticPointerState =
                    Dict.foldl
                        (\id pointerState accum ->
                            if id == pointerEvent.pointerId then
                                accum

                            else
                                Just pointerState
                        )
                        Nothing
                        state.pointers
            in
            case maybeStaticPointerState of
                Just staticPointerState ->
                    let
                        prevDistance =
                            distance movedPointerState.lastPos staticPointerState.lastPos

                        currentDistance =
                            distance pointerEvent.pos staticPointerState.lastPos

                        midpoint =
                            Point2d.midpoint pointerEvent.pos staticPointerState.lastPos

                        scale =
                            currentDistance / prevDistance

                        scaleCmd =
                            model.pinchHandler
                                |> Maybe.map (\handler -> handler { pos = midpoint, scale = scale } val |> Task.Extra.message)
                                |> Maybe.withDefault Cmd.none
                    in
                    ( Model model, scaleCmd )

                _ ->
                    U2.pure (Model model)

        _ ->
            U2.pure (Model model)


{-| Records the position of a pointer event as the last known position against
the pointer state.
-}
recordLastPointerPos : PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
recordLastPointerPos pointerEvent (Model model) =
    let
        state =
            model.state
    in
    case Dict.get pointerEvent.pointerId state.pointers of
        Just pointerState ->
            { model
                | state =
                    { state
                        | pointers =
                            Dict.insert pointerEvent.pointerId
                                { pointerState | lastPos = pointerEvent.pos }
                                state.pointers
                    }
            }
                |> Model
                |> U2.pure

        Nothing ->
            U2.pure (Model model)


clearAllPointerState : Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
clearAllPointerState (Model model) =
    { model
        | state =
            { pointers = Dict.empty
            , gesture = NoPointer
            }
    }
        |> Model
        |> U2.pure


checkForClickOrDouble : a -> PointerEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
checkForClickOrDouble value args (Model model) =
    let
        clickCmd =
            case Dict.get args.button model.buttonHandlers of
                Nothing ->
                    Cmd.none

                Just { click, doubleClick } ->
                    case ( click, doubleClick, args.times ) of
                        ( Just clickHandler, _, 1 ) ->
                            clickHandler { button = args.button, pos = args.pos } value |> Task.Extra.message

                        ( _, Just doubleClickHandler, 2 ) ->
                            doubleClickHandler { button = args.button, pos = args.pos } value
                                |> Task.Extra.message

                        _ ->
                            Cmd.none
    in
    ( Model model, clickCmd )


scaleWheel : a -> WheelEvent coordinates -> Model a msg coordinates -> ( Model a msg coordinates, Cmd msg )
scaleWheel value args (Model model) =
    let
        modeMultiplier =
            case args.deltaMode of
                1 ->
                    40

                2 ->
                    800

                _ ->
                    1

        steps =
            modeMultiplier * args.deltaY / 50 |> clamp -1 1

        scale =
            model.config.mouseWheelZoomStep ^ -steps

        scaleCmd =
            model.wheelHandler
                |> Maybe.map (\handler -> handler { pos = args.pos, scale = scale } value |> Task.Extra.message)
                |> Maybe.withDefault Cmd.none
    in
    ( Model model, scaleCmd )



-- DSL for defining gesture handlers.


type Handlers a msg coordinates
    = Handlers
        { buttonHandlers : Dict Int (ButtonHandler a msg coordinates)
        , wheelHandler : Maybe (ScaleArgs coordinates -> a -> msg)
        , pinchHandler : Maybe (ScaleArgs coordinates -> a -> msg)
        , moveHandler : Maybe (Point2d Pixels coordinates -> a -> msg)
        }


empty : Handlers a msg coordinates
empty =
    Handlers
        { buttonHandlers = Dict.empty
        , wheelHandler = Nothing
        , pinchHandler = Nothing
        , moveHandler = Nothing
        }


apply : Handlers a msg coordinates -> Model a msg coordinates -> Model a msg coordinates
apply (Handlers handlers) (Model model) =
    { model
        | buttonHandlers = handlers.buttonHandlers
        , wheelHandler = handlers.wheelHandler
        , pinchHandler = handlers.pinchHandler
        , moveHandler = handlers.moveHandler
    }
        |> Model


onClick :
    Int
    -> { h | click : PointArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onClick button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithClick =
            { buttonHandlers
                | click = spec.click |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithClick handlers.buttonHandlers
    }
        |> Handlers


onDoubleClick :
    Int
    -> { h | doubleClick : PointArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onDoubleClick button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithDoubleTap =
            { buttonHandlers
                | doubleClick = spec.doubleClick |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithDoubleTap handlers.buttonHandlers
    }
        |> Handlers


onClickAndHold :
    Int
    -> { h | clickAndHold : PointArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onClickAndHold button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithDrag =
            { buttonHandlers
                | clickAndHold = spec.clickAndHold |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithDrag handlers.buttonHandlers
    }
        |> Handlers


onDrag :
    Int
    -> { h | drag : DragArgs coordinates -> a -> msg, dragEnd : DragArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onDrag button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithDrag =
            { buttonHandlers
                | drag = spec.drag |> Just
                , dragEnd = spec.dragEnd |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithDrag handlers.buttonHandlers
    }
        |> Handlers


onDragStart :
    Int
    -> { h | dragStart : DragArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onDragStart button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithDrag =
            { buttonHandlers
                | dragStart = spec.dragStart |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithDrag handlers.buttonHandlers
    }
        |> Handlers


onMove :
    { h | move : Point2d Pixels coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onMove spec (Handlers handlers) =
    { handlers
        | moveHandler = spec.move |> Just
    }
        |> Handlers


onPointerUp :
    Int
    -> { h | pointerUp : PointArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onPointerUp button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithPointerUp =
            { buttonHandlers
                | pointerUp = spec.pointerUp |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithPointerUp handlers.buttonHandlers
    }
        |> Handlers


onPointerDown :
    Int
    -> { h | pointerDown : PointArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onPointerDown button spec (Handlers handlers) =
    let
        buttonHandlers =
            Dict.get button handlers.buttonHandlers
                |> Maybe.withDefault noopHandlers

        buttonHandlersWithPointerDown =
            { buttonHandlers
                | pointerDown = spec.pointerDown |> Just
            }
    in
    { handlers
        | buttonHandlers =
            Dict.insert button buttonHandlersWithPointerDown handlers.buttonHandlers
    }
        |> Handlers


onWheel :
    { h | wheel : ScaleArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onWheel spec (Handlers handlers) =
    { handlers
        | wheelHandler = spec.wheel |> Just
    }
        |> Handlers


onPinch :
    { h | pinch : ScaleArgs coordinates -> a -> msg }
    -> Handlers a msg coordinates
    -> Handlers a msg coordinates
onPinch spec (Handlers handlers) =
    { handlers
        | pinchHandler = spec.pinch |> Just
    }
        |> Handlers



-- Custom event decoders.


withPointerEvent : Decoder a -> (a -> PointerEvent coordinates -> Msg a coordinates) -> Decoder (Msg a coordinates)
withPointerEvent decoder cons =
    Decode.succeed cons
        |> DE.andMap decoder
        |> DE.andMap pointerEventDecoder


withWheel : Decoder a -> (a -> WheelEvent coordinates -> Msg a coordinates) -> Decoder (Msg a coordinates)
withWheel decoder cons =
    Decode.succeed cons
        |> DE.andMap decoder
        |> DE.andMap mouseWheelDecoder



-- Mouse event decoders.


pointerEventDecoder : Decoder (PointerEvent coordinates)
pointerEventDecoder =
    Decode.succeed PointerEvent
        |> DE.andMap (Decode.field "pointerId" Decode.int)
        |> DE.andMap (Decode.field "button" Decode.int)
        |> DE.andMap clientPosDecoder
        |> DE.andMap (Decode.field "detail" Decode.int)


clientPosDecoder : Decoder (Point2d Pixels coordinates)
clientPosDecoder =
    Decode.succeed Point2d.pixels
        |> DE.andMap (Decode.field "clientX" Decode.float)
        |> DE.andMap (Decode.field "clientY" Decode.float)


mouseWheelDecoder : Decoder (WheelEvent coordinates)
mouseWheelDecoder =
    Decode.map3 WheelEvent
        clientPosDecoder
        (Decode.field "deltaY" Decode.float)
        (Decode.field "deltaMode" Decode.int)



-- Helpers


noopHandlers : ButtonHandler a msg coordinates
noopHandlers =
    { click = Nothing
    , doubleClick = Nothing
    , clickAndHold = Nothing
    , drag = Nothing
    , dragStart = Nothing
    , dragEnd = Nothing
    , move = Nothing
    , pointerUp = Nothing
    , pointerDown = Nothing
    }


{-| Produces a list of buttons that are down and have handlers set for them.
-}
activeButtonsWithHandlers : Model a msg coordinates -> List Int
activeButtonsWithHandlers (Model model) =
    Dict.foldl
        (\pointerId state accum ->
            case Dict.get state.button model.buttonHandlers of
                Just _ ->
                    state.button :: accum

                Nothing ->
                    accum
        )
        []
        model.state.pointers


distance : Point2d Pixels coordinates -> Point2d Pixels coordinates -> Float
distance a b =
    Point2d.distanceFrom a b |> Pixels.toFloat


atLeastOneMoveHandler : Model a msg coordinates -> Bool
atLeastOneMoveHandler (Model model) =
    List.length (activeButtonsWithHandlers (Model model)) > 0 || Maybe.Extra.isJust model.moveHandler


atLeastOnePointerDownHandler : Model a msg coordinates -> Bool
atLeastOnePointerDownHandler (Model model) =
    handlerCheck model .pointerDown
        || handlerCheck model .dragStart
        || handlerCheck model .drag


atLeastOnePointerUpHandler : Model a msg coordinates -> Bool
atLeastOnePointerUpHandler (Model model) =
    handlerCheck model .pointerUp
        || handlerCheck model .dragStart
        || handlerCheck model .drag


atLeastOneClickHandler : Model a msg coordinates -> Bool
atLeastOneClickHandler (Model model) =
    handlerCheck model .click


handlerCheck : { m | buttonHandlers : Dict Int (ButtonHandler a msg coordinates) } -> (ButtonHandler a msg coordinates -> Maybe b) -> Bool
handlerCheck model recFn =
    Dict.foldl
        (\_ bh accum ->
            case recFn bh of
                Just _ ->
                    True

                _ ->
                    accum
        )
        False
        model.buttonHandlers