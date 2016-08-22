module Eventloop.Module.Timer.Timer
    ( setupTimerModuleConfiguration
    , timerModuleIdentifier
    , timerInitializer
    , timerEventRetriever
    , timerEventSender
    , timerTeardown
    ) where

import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import Data.Maybe
import Data.List

import Eventloop.Module.Timer.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System


setupTimerModuleConfiguration :: EventloopSetupModuleConfiguration
setupTimerModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                        timerModuleIdentifier
                                        (Just timerInitializer)
                                        (Just timerEventRetriever)
                                        Nothing
                                        Nothing
                                        (Just timerEventSender)
                                        (Just timerTeardown)
                                 )

timerModuleIdentifier :: EventloopModuleIdentifier
timerModuleIdentifier = "timer"

                             
timerInitializer :: Initializer
timerInitializer sharedConst sharedIO
    = do
        inQueue <- createBlockingConcurrentQueue
        return (sharedConst, sharedIO, TimerConstants inQueue, TimerState [] [])
                        
                                
timerEventRetriever :: EventRetriever
timerEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        inTicks <- takeAllFromBlockingConcurrentQueue inQueue
        ioState <- readTVarIO ioStateT -- This first read is just a snapshot
        let
            toStop = map (\(Tick id) -> id) inTicks
            startedTimers_  = startedTimers ioState
        sequence $ map (haltTimer startedTimers_) toStop
        atomically $ do
            ioState' <- readTVar ioStateT
            let
                startedTimers_'  = startedTimers ioState'
                startedTimers_'' = foldl unregisterTimer startedTimers_' toStop
            writeTVar ioStateT (ioState'{startedTimers = startedTimers_'})

        return (map InTimer inTicks)
    where
        inQueue = tickInQueue ioConst


timerEventSender :: EventSender
timerEventSender _ _ _ _ Stop = return ()
timerEventSender sharedConst sharedIOT ioConst ioStateT (OutTimer a)
    = timerEventSender' ioStateT tickBuffer a
    where
        tickBuffer = tickInQueue ioConst

                                    
timerEventSender' :: TVar IOState -> TickBuffer -> TimerOut -> IO ()
timerEventSender' ioStateT tickBuffer (SetTimer id delay)
    = do
        startedTimer <- startTimer tickBuffer id delay (oneShotStart)
        atomically $ do
            ioState <- readTVar ioStateT
            let
                startedTimers_' = registerTimer (startedTimers ioState) startedTimer
            writeTVar ioStateT ioState{startedTimers=startedTimers_'}

timerEventSender' ioStateT tickBuffer (SetIntervalTimer id delay)
    = do
        startedTimer <- startTimer tickBuffer id delay (repeatedStart)
        atomically $ do
            ioState <- readTVar ioStateT
            let
                startedITimers_' = registerTimer (startedIntervalTimers ioState) startedTimer
            writeTVar ioStateT ioState{startedIntervalTimers=startedITimers_'}

timerEventSender' ioStateT tickBuffer (UnsetTimer id)
    = do
        ioState <- readTVarIO ioStateT -- This first read is just a snapshot
        let
            startedTimers_  = startedTimers ioState
            startedITimers_ = startedIntervalTimers ioState
        haltTimer startedTimers_ id
        haltTimer startedITimers_ id
        atomically $ do
            ioState' <- readTVar ioStateT
            let
                startedTimers_'  = startedTimers ioState'
                startedITimers_' = startedIntervalTimers ioState'
            writeTVar ioStateT ioState'{ startedTimers = startedTimers_'
                                       , startedIntervalTimers = startedITimers_'
                                       }

           
timerTeardown :: Teardown
timerTeardown sharedConst sharedIO ioConst ioState
    = do
        let
            allStartedTimers = (startedTimers ioState) ++ (startedIntervalTimers ioState)
            allStartedIds = map fst allStartedTimers
        sequence_ $ map (haltTimer allStartedTimers) allStartedIds
        return sharedIO

           
registerTimer :: [StartedTimer] -> StartedTimer -> [StartedTimer]
registerTimer startedTimers startedTimer
    = startedTimers ++ [startedTimer]


startTimer :: TickBuffer -> TimerId -> MicroSecondDelay -> TimerStartFunction -> IO StartedTimer
startTimer incTickBuff id delay startFunc
    = do
        timer <- newTimer
        startFunc timer (tick id incTickBuff) ((usDelay.fromIntegral) delay)
        return (id, timer)


unregisterTimer :: [StartedTimer] -> TimerId -> [StartedTimer]
unregisterTimer startedTimers id
    = filter (\(id', _) -> id /= id') startedTimers


haltTimer :: [StartedTimer] -> TimerId -> IO ()
haltTimer startedTimers id
    = do
        let
            startedTimerM = findStartedTimer startedTimers id
            stopAction (Just (_, timer)) = stopTimer timer
            stopAction Nothing           = return ()
        stopAction startedTimerM


findStartedTimer :: [StartedTimer] -> TimerId -> Maybe StartedTimer
findStartedTimer startedTimers id = find (\(id', timer) -> id == id') startedTimers
                                        
       
tick :: TimerId -> TickBuffer -> IO ()
tick id tickBuffer = putInBlockingConcurrentQueue tickBuffer (Tick id)