module Eventloop.Module.StatefulGraphics.StatefulGraphics
    ( setupStatefulGraphicsModuleConfiguration
    , statefulGraphicsModuleIdentifier
    , statefulGraphicsInitializer
    , statefulGraphicsPostProcessor
    , statefulGraphicsTeardown
    ) where

import Eventloop.Module.StatefulGraphics.Types

import Eventloop.Module.Websocket.Canvas
import Eventloop.Module.BasicShapes
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

import Data.Maybe

import Debug.Trace

setupStatefulGraphicsModuleConfiguration :: EventloopSetupModuleConfiguration
setupStatefulGraphicsModuleConfiguration = ( EventloopSetupModuleConfiguration
                                              statefulGraphicsModuleIdentifier
                                              (Just statefulGraphicsInitializer)
                                              Nothing
                                              Nothing
                                              (Just statefulGraphicsPostProcessor)
                                              Nothing
                                              (Just statefulGraphicsTeardown)
                                          )


statefulGraphicsModuleIdentifier :: EventloopModuleIdentifier
statefulGraphicsModuleIdentifier = "statefulgraphics"


statefulGraphicsInitializer :: Initializer
statefulGraphicsInitializer sharedIO
    = return (sharedIO, StatefulGraphicsState [])


statefulGraphicsTeardown :: Teardown
statefulGraphicsTeardown sharedIO statefulState
    = return (sharedIO)


statefulGraphicsPostProcessor :: PostProcessor
statefulGraphicsPostProcessor shared (StatefulGraphicsState states) (OutStatefulGraphics canvasId commands)
    = return (shared, StatefulGraphicsState states', newScene)
    where
        (state', newScene) = calculateNewScene canvasId state commands
        stateM = findGraphicalState states canvasId
        state = case stateM of
                    Just state_ -> state_
                    Nothing -> []
        states' = replaceGraphicalState states canvasId state'

statefulGraphicsPostProcessor shared iostate out
    = return (shared, iostate, [out])


replaceGraphicalState :: GraphicsStates -> CanvasId -> GraphicsState -> GraphicsStates
replaceGraphicalState [] id state = [(id, state)]
replaceGraphicalState ((id, state):states) canvasId newState
    | id == canvasId = (canvasId, newState):states
    | otherwise = (id, state):(replaceGraphicalState states canvasId newState)


findGraphicalState :: GraphicsStates -> CanvasId -> Maybe GraphicsState
findGraphicalState [] _ = Nothing
findGraphicalState ((id, state):states) canvasId
    | id == canvasId = Just state
    | otherwise =  findGraphicalState states canvasId


calculateNewScene :: CanvasId -> GraphicsState -> [StatefulGraphicsOut] -> (GraphicsState, [Out])
calculateNewScene canvasId state outs
    = trace "Calculate new scene" (state', [clearCanvas, OutBasicShapes $ DrawShapes canvasId basicShapes])
    where
        (state', performed) = trace ("Outs: " ++ (show $ length outs)) foldl foldPerform (state, []) outs
        foldPerform (state_, performed_) statefulOut = (state_', performed_ ++ [performed_'])
            where
                (state_', performed_') = performStatefulGraphicsOut state_ statefulOut

        clearCanvas = OutCanvas $ CanvasOperations canvasId [Clear ClearCanvas]
        basicShapes = map snd state'


performStatefulGraphicsOut :: GraphicsState -> StatefulGraphicsOut -> (GraphicsState, GraphicPerformed)
performStatefulGraphicsOut state (Draw statefulGraphic)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> trace ("Modified: " ++ (show $ fst statefulGraphic)) (state', Modified statefulGraphic)
        Nothing                 -> trace ("Drawn: " ++ (show $ fst statefulGraphic)) (state', Drawn statefulGraphic)
    where
        (state', oldStatefulGraphicM) = addOrReplaceGraphics state statefulGraphic
performStatefulGraphicsOut state (Remove id)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> trace ("Removed: " ++ (show id)) (state', Removed oldStatefulGraphic)
        Nothing                 -> trace ("NoOp: " ++ (show id)) (state , NoOp)
    where
        (state', oldStatefulGraphicM) = removeGraphics state id


addOrReplaceGraphics :: GraphicsState -> StatefulGraphic -> (GraphicsState, (Maybe StatefulGraphic))
addOrReplaceGraphics [] new = ([new], Nothing) -- Add action
addOrReplaceGraphics (old@(id, _):state) new@(id', newGraphic)
    | id == id' = (new:state, Just old) -- Update action
    | otherwise = (old:state', result)
    where
        (state', result) = addOrReplaceGraphics state new


removeGraphics :: GraphicsState -> NamedId -> (GraphicsState, (Maybe StatefulGraphic))
removeGraphics [] _ = ([], Nothing)
removeGraphics (sg@(id, _):state) id'
    | id == id' = (state, Just sg)
    | otherwise = (sg:state', result)
    where
        (state', result) = removeGraphics state id'




