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
import Eventloop.Utility.Vectors


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




getId :: StatefulBB -> NamedId
getId (StatefulBB (Stateful id _ _) _) = id

getZ :: StatefulBB -> ZIndex
getZ (StatefulBB (Stateful _ z _) _) = z

getShape :: StatefulBB -> Shape
getShape (StatefulBB (Stateful _ _ shape) _) = shape


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
    = (state', [ OutCanvas $ CanvasOperations canvasId (map Clear removes)
               , OutBasicShapes $ DrawShapes canvasId basicShapes
               ]
      )
    where
        (state', performed) = foldl foldPerform (state, []) outs
        foldPerform (state_, performed_) statefulOut = (state_', performed_ ++ [performed_'])
            where
                (state_', performed_') = performStatefulGraphicsOut state_ statefulOut
        (toRedraw, toRemove) = calculateRedraws state' performed
        basicShapes = map getShape toRedraw
        removes = map calculateRemove toRemove

{-
Uses yMin as canvas treats rectangle from their lower left corner
-}
calculateRemove :: StatefulBB
                -> ClearPart
calculateRemove (StatefulBB _ bb)
    = ClearRectangle ((floor $ xMin bb) - 1, (floor $ yMin bb) - 1) (ceiling width + 2, ceiling height + 2)
    where
        height = (yMax bb) - (yMin bb)
        width = (xMax bb) - (xMin bb)


calculateRedraws :: GraphicsState
                 -> [GraphicPerformed]
                 -> (GraphicsState, GraphicsState) -- Redraw and Remove state
calculateRedraws _ [] = ([], [])
calculateRedraws state ((Drawn sbb@(StatefulBB (Stateful name _ _) _)):performed)
    = (toRedraw', toRemove')
    where
        id = getId sbb
        (toRedraw, toRemove) = calculateRedraws state performed
        (_, toRedraw', toRemove') = calculateRedrawsForDrawn (state, toRedraw, toRemove) sbb

calculateRedraws state ((Removed sg@(StatefulBB (Stateful name _ _) _)):performed)
    = (toRedraw', toRemove')
    where
        (toRedraw, toRemove) = calculateRedraws state performed
        (_, toRedraw', toRemove') = calculateRedrawsForRemoved (state, toRedraw, toRemove) sg

calculateRedraws state ((Modified old_sg new_sg):performed)
    --  noDimChange oldGraphic newGraphic = calculateDraws toCheck toRedraw (Drawn sg) TODO Modified -> Drawn optimalization
    = calculateRedraws state ((Removed old_sg):(Drawn new_sg):performed)

calculateRedraws state (NoOp:performed)
    = calculateRedraws state performed


{-
Only redraw when a graphic if the drawn graphic overlaps or if a graphic is above and contained by the drawn
graphic. Also add the drawn graphic to redraw.
However, if there is one graphic to be found that completely contains the drawn graphic, nothing has to happen.
Also, all graphics completely behind the drawn graphic, will not be redrawn.
-}
calculateRedrawsForDrawn :: (GraphicsState, GraphicsState, GraphicsState) -- Current check and redraw state
                         -> StatefulBB
                         -> (GraphicsState, GraphicsState, GraphicsState)
calculateRedrawsForDrawn (toCheck, toRedraw, toRemove) new@(StatefulBB (Stateful id _ (Text {})) _)
   = calculateRedrawsForRemoved (toCheck', new:toRedraw, toRemove) new
   where
       toCheck' = fst $ removeGraphic toCheck id
calculateRedrawsForDrawn (toCheck, toRedraw, toRemove) new
    = foldl calculateRedrawsForDrawn (toCheck', toRedraw', toRemove) checkNow
    where
        id = getId new
        z  = getZ new
        (below, _, above) = splitOn (\sbb -> getId sbb == id) toCheck -- Find which graphics are above and below toProcess

        aboveOverlapped  = filter (overlaps new) above
        aboveContained   = filter (contains new) above
        aboveContainedBy = filter (\sg -> contains sg new) above
        aboveNotTouching = filter (not.(touches new)) above

        toCheck' = fst $ removeGraphic toCheck id
        toRedraw' = fst $ addOrReplaceGraphic toRedraw new

        checkNow = aboveOverlapped ++ aboveContained ++ aboveContainedBy


calculateRedrawsForRemoved :: (GraphicsState, GraphicsState, GraphicsState) -- Current check and redraw state
                           -> StatefulBB
                           -> (GraphicsState, GraphicsState, GraphicsState)
calculateRedrawsForRemoved (toCheck, toRedraw, toRemove) old
    = (toCheck', toRedraw', toRemove')
    where
        id = getId old
        z = getZ old

        (below, above) = split (\sbb -> getZ sbb > z) toCheck -- Find which graphics are above and below toProcess

        belowOverlapped  = filter (overlaps old) below
        belowContained   = filter (contains old) below
        belowContainedBy = filter (\sg -> contains sg old) below

        aboveOverlapped  = filter (overlaps old) above
        aboveContained   = filter (contains old) above
        aboveContainedBy = filter (\sg -> contains sg old) above

        checkNow = belowOverlapped
                 ++ belowContained
                 ++ belowContainedBy
                 ++ aboveOverlapped
                 ++ aboveContained
                 ++ aboveContainedBy
        (toCheck', toRedraw', toRemove') = foldl calculateRedrawsForDrawn (toCheck, toRedraw, old:toRemove) checkNow


statefulIds :: GraphicsState -> [NamedId]
statefulIds = map getId


statefulId :: StatefulGraphic -> NamedId
statefulId (Stateful id _ _) = id


split :: (a -> Bool) -> [a] -> ([a], [a])
split _  []   = ([], [])
split on (x:xs)
    | on x      = ([], x:xs)
    | otherwise = (x:xs', xs'')
    where
        (xs', xs'') = split on xs


splitOn :: (a -> Bool) -> [a] -> ([a], Maybe a, [a])
splitOn _  []   = ([], Nothing, [])
splitOn on (x:xs)
    | on x      = ([], Just x, xs)
    | otherwise = (x:xs', x', xs'')
    where
        (xs', x', xs'') = splitOn on xs


performStatefulGraphicsOut :: GraphicsState -> StatefulGraphicsOut -> (GraphicsState, GraphicPerformed)
performStatefulGraphicsOut state (Draw statefulGraphic)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> (state', Modified oldStatefulGraphic statefulBB)
        Nothing                 -> (state', Drawn statefulBB)
    where
        statefulBB = (StatefulBB statefulGraphic bb)
        (state', oldStatefulGraphicM) = addOrReplaceGraphic state statefulBB
        bb = toBoundingBox statefulGraphic

performStatefulGraphicsOut state (Remove id)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> (state', Removed oldStatefulGraphic)
        Nothing                 -> (state , NoOp)
    where
        (state', oldStatefulGraphicM) = removeGraphic state id


addGraphic :: GraphicsState -> StatefulBB -> GraphicsState
addGraphic [] new = [new]
addGraphic (graphic:state) new
    | z > z' = new:graphic:state
    | otherwise = graphic:(addGraphic state new)
    where
        z  = getZ graphic
        z' = getZ new


addOrReplaceGraphic :: GraphicsState -> StatefulBB -> (GraphicsState, (Maybe StatefulBB))
addOrReplaceGraphic [] new = ([new], Nothing) -- Add action
addOrReplaceGraphic (graphic:state) new
    | id == id' && z == z' = (new:state, Just graphic)                         -- Simple update

    | id == id' && z /= z' = let                                               -- Update action (must be z <= z')
                                state' = addGraphic state new                  -- Insert new higher up
                             in
                             (state', Just graphic)                            -- new is higher, forget current=old

    | id /= id' && z > z'  = let                                               -- Add or update action
                                (state', old) = removeGraphic state id'       -- Search higher state for possible stale
                             in
                             (new:graphic:state', old)

    | otherwise            = let                                               -- Search further
                                (state', result) = addOrReplaceGraphic state new
                             in
                             (graphic:state', result)
    where
        id  = getId graphic
        z   = getZ graphic
        id' = getId new
        z'  = getZ new


removeGraphic :: GraphicsState -> NamedId -> (GraphicsState, (Maybe StatefulBB))
removeGraphic [] _ = ([], Nothing)
removeGraphic (sg:state) id'
    | id == id' = (state, Just sg)
    | otherwise = (sg:state', result)
    where
        (state', result) = removeGraphic state id'
        id = getId sg
