module Eventloop.Module.StatefulGraphics.StatefulGraphics
    ( setupStatefulGraphicsModuleConfiguration
    , statefulGraphicsModuleIdentifier
    , statefulGraphicsInitializer
    , statefulGraphicsPostProcessor
    , statefulGraphicsTeardown
    ) where

import Control.Concurrent.STM
import Data.Maybe

import Eventloop.Module.StatefulGraphics.Types

import Eventloop.Module.Websocket.Canvas
import Eventloop.Module.BasicShapes
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

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
statefulGraphicsInitializer sharedConst sharedIO
    = return (sharedConst, sharedIO, NoConstants, StatefulGraphicsState [])


statefulGraphicsTeardown :: Teardown
statefulGraphicsTeardown sharedConst sharedIO ioConst ioState
    = return (sharedIO)


statefulGraphicsPostProcessor :: PostProcessor
statefulGraphicsPostProcessor sharedConst sharedIOT ioConst ioStateT (OutStatefulGraphics canvasId commands)
    = atomically $ do
        (StatefulGraphicsState states) <- readTVar ioStateT
        let
            stateM = findGraphicalState states canvasId
            state = case stateM of
                        Just state_ -> state_
                        Nothing -> []
            (state', newScene) = calculateNewScene canvasId state commands
            states' = replaceGraphicalState states canvasId state'
        writeTVar ioStateT (StatefulGraphicsState states')
        return newScene


statefulGraphicsPostProcessor sharedConst sharedIOT ioConst ioStateT out
    = return [out]


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
    = (state', [clearCanvas, OutBasicShapes $ DrawShapes canvasId basicShapes])
    where
        (state', performed) = foldl foldPerform (state, []) outs
        foldPerform (state_, performed_) statefulOut = (state_', performed_ ++ [performed_'])
            where
                (state_', performed_') = performStatefulGraphicsOut state_ statefulOut

        clearCanvas = OutCanvas $ CanvasOperations canvasId [Clear ClearCanvas]
        basicShapes = map snd state'


performStatefulGraphicsOut :: GraphicsState -> StatefulGraphicsOut -> (GraphicsState, GraphicPerformed)
performStatefulGraphicsOut state (Draw statefulGraphic)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> (state', Modified statefulGraphic)
        Nothing                 -> (state', Drawn statefulGraphic)
    where
        (state', oldStatefulGraphicM) = addOrReplaceGraphics state statefulGraphic
performStatefulGraphicsOut state (Remove id)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> (state', Removed oldStatefulGraphic)
        Nothing                 -> (state , NoOp)
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