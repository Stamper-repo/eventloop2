module Eventloop.Module.Timer.Timer
    ( setupTimerModuleConfiguration
    , timerModuleIdentifier
    , timerInitializer
    , timerEventRetriever
    , timerEventSender
    , timerTeardown
    ) where

import Control.Concurrent.Timer
import Control.Concurrent.MVar
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
timerInitializer sharedIO = do
                                incTickBuff <- newMVar []
                                incITickBuff <- newMVar []
                                return (sharedIO, TimerState [] [] incITickBuff incTickBuff)
                        
                                
timerEventRetriever :: EventRetriever
timerEventRetriever sharedIO timerState = do
                                            let
                                                incTickBuff = incomingTickBuffer timerState
                                                incITickBuff = incomingIntervalTickBuffer timerState
                                            incTicks <- swapMVar incTickBuff []
                                            incITicks <- swapMVar incITickBuff []
                                            let
                                                tickedTimerIds = map (\(Tick id) -> id) incTicks
                                            startedTimers' <- foldr (\id startedTimersIO -> (startedTimersIO >>= unregisterTimer id)) (return $ startedTimers timerState) tickedTimerIds
                                            return (sharedIO, timerState{startedTimers = startedTimers'}, (map InTimer incTicks) ++ (map InTimer incITicks))
                                    

timerEventSender :: EventSender
timerEventSender sharedIO timerState (OutTimer a) = do
                                                        timerState' <- timerEventSender' timerState a
                                                        return (sharedIO, timerState')
                                    
timerEventSender' :: IOState -> TimerOut -> IO IOState
timerEventSender' timerState (SetTimer id delay) = do
                                                    let
                                                        incTickBuff = incomingTickBuffer timerState
                                                        startedTimers_ = startedTimers timerState
                                                    startedTimers' <- registerTimer startedTimers_ incTickBuff id delay (oneShotStart)
                                                    let
                                                        timerState' = timerState {startedTimers = startedTimers'}
                                                    return timerState'

timerEventSender' timerState (SetIntervalTimer id delay) = do
                                                            let
                                                                incITickBuff = incomingIntervalTickBuffer timerState
                                                                startedITimers = startedIntervalTimers timerState
                                                            startedITimers' <- registerTimer startedITimers incITickBuff id delay (repeatedStart)
                                                            let
                                                                timerState' = timerState {startedIntervalTimers = startedITimers'}
                                                            return timerState'
                                                            
timerEventSender' timerState (UnsetTimer id) = do
                                                startedTimers' <- unregisterTimer id (startedTimers timerState)
                                                startedITimers' <- unregisterTimer id (startedIntervalTimers timerState)
                                                return (timerState {startedTimers = startedTimers', startedIntervalTimers=startedITimers'})
           
           
timerTeardown :: Teardown
timerTeardown sharedIO timerState = do
                                        let
                                            allStartedTimers = (startedTimers timerState) ++ (startedIntervalTimers timerState)
                                            allStartedIds = map fst allStartedTimers
                                        sequence_ (map (\id -> unregisterTimer id allStartedTimers) allStartedIds)
                                        return (sharedIO)

           
registerTimer :: [StartedTimer] -> IncomingTickBuffer -> TimerId -> MicroSecondDelay -> TimerStartFunction -> IO [StartedTimer]
registerTimer startedTimers incTickBuff id delay startFunc = do
                                                                timer <- newTimer
                                                                startFunc timer (tick id incTickBuff) ((usDelay.fromIntegral) delay)
                                                                return (startedTimers ++ [(id, timer)])
       

unregisterTimer :: TimerId -> [StartedTimer]  -> IO [StartedTimer]
unregisterTimer id startedTimers = do
                                    let
                                        startedTimerM = findStartedTimer startedTimers id
                                        stopAction (Just (_, timer)) = stopTimer timer
                                        stopAction Nothing           = return () 
                                        startedTimers' = filter (\(id', _) -> id /= id') startedTimers
                                    stopAction startedTimerM
                                    return startedTimers'
                                    

findStartedTimer :: [StartedTimer] -> TimerId -> Maybe StartedTimer
findStartedTimer startedTimers id = find (\(id', timer) -> id == id') startedTimers
                                        
       
tick :: TimerId -> IncomingTickBuffer -> IO ()
tick id incTickBuff = modifyMVar_ incTickBuff (\ticks -> return $ ticks ++ [Tick id])