module Eventloop.Module.StatefulGraphics.StatefulGraphics
    ( setupStatefulGraphicsModuleConfiguration
    , statefulGraphicsModuleIdentifier
    , statefulGraphicsInitializer
    , statefulGraphicsPostProcessor
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
                                              Nothing
                                          )


statefulGraphicsModuleIdentifier :: EventloopModuleIdentifier
statefulGraphicsModuleIdentifier = "statefulgraphics"


statefulGraphicsInitializer :: Initializer
statefulGraphicsInitializer sharedConst sharedIO
    = return (sharedConst, sharedIO, NoConstants, StatefulGraphicsState [])


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
    = (state', [ OutCanvas $ CanvasOperations canvasId [Clear ClearCanvas]
               , OutBasicShapes $ DrawShapes canvasId basicShapes
               , OutCanvas $ CanvasOperations canvasId [Frame]
               ]
      )
    where
        (state', performed) = foldl foldPerform (state, []) outs
        foldPerform (state_, performed_) statefulOut = (state_', performed_ ++ [performed_'])
            where
                (state_', performed_') = performStatefulGraphicsOut state_ statefulOut
        basicShapes = map (\(_, _, shape) -> shape) state'


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
addOrReplaceGraphics (graphic@(id, z, _):state) new@(id', z', newGraphic)
    | id == id' && z == z' = (new:state, Just graphic)                         -- Simple update

    | id == id' && z /= z' = let                                               -- Update action (must be z <= z')
                                (state', _) = addOrReplaceGraphics state new   -- Insert new higher up
                             in
                             (state', Just graphic)                            -- new is higher, forget current=old

    | id /= id' && z > z'  = let                                               -- Add or update action
                                (state', old) = removeGraphics state id'       -- Search higher state for possible stale
                             in
                             (new:graphic:state', old)

    | otherwise            = let                                               -- Search further
                                (state', result) = addOrReplaceGraphics state new
                             in
                             (graphic:state', result)


removeGraphics :: GraphicsState -> NamedId -> (GraphicsState, (Maybe StatefulGraphic))
removeGraphics [] _ = ([], Nothing)
removeGraphics (sg@(id, _, _):state) id'
    | id == id' = (state, Just sg)
    | otherwise = (sg:state', result)
    where
        (state', result) = removeGraphics state id'