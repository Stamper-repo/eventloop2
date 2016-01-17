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

import Debug.Trace


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
        toRedrawIds = statefulIds toRedraw
        toRemoveIds = trace ("To Redraw: " ++ (show toRedrawIds)) statefulIds toRemove
        basicShapes = trace ("To Remove: " ++ (show toRemoveIds)) (map (\(Stateful _ _ shape) -> shape) toRedraw)
        removes = map calculateRemove toRemove

{-
Uses yMin as canvas treats rectangle from their lower left corner
-}
calculateRemove :: StatefulGraphic
                -> ClearPart
calculateRemove (Stateful _ _ graphic)
    = ClearRectangle (round $ xMin bb, round $ yMin bb) (round width, round height)
    where
        bb = toBoundingBox graphic
        height = (yMax bb) - (yMin bb)
        width = (xMax bb) - (xMin bb)


calculateRedraws :: GraphicsState
                 -> [GraphicPerformed]
                 -> (GraphicsState, GraphicsState) -- Redraw and Remove state
calculateRedraws _ [] = ([], [])
calculateRedraws state ((Drawn sg@(Stateful id _ _)):performed)
    = (toRedraw', toRemove)
    where
        (toRedraw, toRemove) = calculateRedraws state performed
        (_, toRedraw') = calculateRedrawsForDrawn (state, toRedraw) sg

calculateRedraws state ((Removed sg):performed)
    = (toRedraw', toRemove')
    where
        (toRedraw, toRemove) = calculateRedraws state performed
        (_, toRedraw', toRemove') = calculateRedrawsForRemoved (state, toRedraw, toRemove) sg

calculateRedraws state ((Modified sg):performed)
    -- | noDimChange oldGraphic newGraphic = calculateDraws toCheck toRedraw (Drawn sg) TODO Modified -> Drawn optimalization
    = calculateRedraws state ((Removed sg):(Drawn sg):performed)

calculateRedraws state (NoOp:performed)
    = calculateRedraws state performed


{-
Only redraw when a graphic if the drawn graphic overlaps or if a graphic is above and contained by the drawn
graphic. Also add the drawn graphic to redraw.
However, if there is one graphic to be found that completely contains the drawn graphic, nothing has to happen.
Also, all graphics completely behind the drawn graphic, will not be redrawn.
-}
calculateRedrawsForDrawn :: (GraphicsState, GraphicsState) -- Current check and redraw state
                         -> StatefulGraphic
                         -> (GraphicsState, GraphicsState)
calculateRedrawsForDrawn (toCheck, toRedraw) new@(Stateful id z newGraphic)
    = foldl calculateRedrawsForDrawn (toCheck', toRedraw') checkNow
    where
        (below, _, above) = splitOn (\(Stateful id' _ _) -> id' == id) toCheck -- Find which graphics are above and below toProcess

        aboveOverlapped  = filter (overlaps new) above
        aboveContained   = filter (contains new) above
        aboveContainedBy = filter (\sg -> contains sg new) above
        aboveNotTouching = filter (not.(touches new)) above

        toCheck' = fst $ removeGraphic toCheck id
        toRedraw' = fst $ addOrReplaceGraphic toRedraw new

        checkNow = aboveOverlapped ++ aboveContained ++ aboveContainedBy


calculateRedrawsForRemoved :: (GraphicsState, GraphicsState, GraphicsState) -- Current check and redraw state
                           -> StatefulGraphic
                           -> (GraphicsState, GraphicsState, GraphicsState)
calculateRedrawsForRemoved (toCheck, toRedraw, toRemove) old@(Stateful id z oldGraphic)
    = (toCheck', toRedraw', toRemove')
    where
        (below, above) = split (\(Stateful _ z' _) -> z' > z) toCheck -- Find which graphics are above and below toProcess

        belowOverlapped  = filter (overlaps old) below
        belowContained   = filter (contains old) below
        belowContainedBy = filter (\sg -> contains sg old) below

        aboveOverlapped  = filter (overlaps old) above
        aboveContained   = filter (contains old) above
        aboveContainedBy = filter (\sg -> contains sg old) above

        toRemove' = old:toRemove

        checkNow = belowOverlapped
                 ++ belowContained
                 ++ belowContainedBy
                 ++ aboveOverlapped
                 ++ aboveContained
                 ++ aboveContainedBy
        (toCheck', toRedraw') = foldl calculateRedrawsForDrawn (toCheck, toRedraw) checkNow


statefulIds :: [StatefulGraphic] -> [NamedId]
statefulIds = map statefulId


statefulId :: StatefulGraphic -> NamedId
statefulId (Stateful id _ _) = id

{-
calculateDraws (toCheck, toRedraw, toRemove) (Removed old@(id, z, oldGraphic))
    | not null aboveContainedBy = (toCheck, toRedraw, toRemove) -- TODO
    | otherwise                 =
    where
        (below, fstAbove, otherAbove) = splitOn (\(_, z', _) -> z' > z) toCheck -- Find which graphics are above and below toProcess
        above = fstAbove:otherAbove
        aboveContainedBy = filter (\sg -> contains sg old) above
        belowContainedBy = filter(\sg -> contains sg old) below
        contained = filter (contains old) toCheck
-}

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
        Just oldStatefulGraphic -> (state', Modified statefulGraphic)
        Nothing                 -> (state', Drawn statefulGraphic)
    where
        (state', oldStatefulGraphicM) = addOrReplaceGraphic state statefulGraphic
performStatefulGraphicsOut state (Remove id)
    = case oldStatefulGraphicM of
        Just oldStatefulGraphic -> (state', Removed oldStatefulGraphic)
        Nothing                 -> (state , NoOp)
    where
        (state', oldStatefulGraphicM) = removeGraphic state id


addGraphic :: GraphicsState -> StatefulGraphic -> GraphicsState
addGraphic [] new = [new]
addGraphic (graphic@(Stateful _ z _):state) new@(Stateful _ z' _)
    | z > z' = new:graphic:state
    | otherwise = graphic:(addGraphic state new)


addOrReplaceGraphic :: GraphicsState -> StatefulGraphic -> (GraphicsState, (Maybe StatefulGraphic))
addOrReplaceGraphic [] new = ([new], Nothing) -- Add action
addOrReplaceGraphic (graphic@(Stateful id z _):state) new@(Stateful id' z' newGraphic)
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


removeGraphic :: GraphicsState -> NamedId -> (GraphicsState, (Maybe StatefulGraphic))
removeGraphic [] _ = ([], Nothing)
removeGraphic (sg@(Stateful id _ _):state) id'
    | id == id' = (state, Just sg)
    | otherwise = (sg:state', result)
    where
        (state', result) = removeGraphic state id'
