module Eventloop.System.EventloopThread where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Concurrent.ExceptionUtility
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Data.Maybe

import Eventloop.System.Processing
import Eventloop.Types.Common
import Eventloop.Types.Exception
import Eventloop.Types.Events
import Eventloop.Types.System

startEventlooping :: EventloopSystemConfiguration progstateT 
                  -> IO ()
startEventlooping systemConfig
    = handle
        ( \exception ->
            case (fromException exception) of
                (Just RequestShutdownException) -> throwIO RequestShutdownException
                _                               -> throwIO (EventloopException exception)
        )
        ( do
            putInBlockingConcurrentQueue inEventQueue_ Start
            forever $ do
              inEvent <- takeFromBlockingConcurrentQueue inEventQueue_ -- Take an In event
              processedInEvents <- processEvents "Preprocessing" systemConfig modulePreprocessors [inEvent] -- Preprocess it
              outEvents <- eventloopSteps eventloop progstateT_ processedInEvents  -- Eventloop over the preprocessed In events
              processedOutEvents <- processEvents "Postprocessing" systemConfig modulePostprocessors outEvents -- Postprocess the Out events
              evaluatedOutEvents <- evaluate $ force processedOutEvents
              putAllInBlockingConcurrentQueue outEventQueue_ processedOutEvents -- Send the processed Out events to the OutRouter
        )
    where
        eventloopConfig_ = eventloopConfig systemConfig
        eventloop = eventloopFunc eventloopConfig_
        progstateT_ = progstateT eventloopConfig_
        inEventQueue_ = inEventQueue eventloopConfig_
        outEventQueue_ = outEventQueue eventloopConfig_
        moduleConfigurations_ = moduleConfigs systemConfig
        modulePreprocessors = findProcessors moduleConfigurations_ preprocessorM
        modulePostprocessors = findProcessors moduleConfigurations_ postprocessorM

        
findProcessors :: [EventloopModuleConfiguration]
               -> (EventloopModuleConfiguration -> Maybe (SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> event -> IO [event])) -- Pre-/Postprocessor function
               -> [(EventloopModuleIdentifier, IOConstants, TVar IOState, (SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> event -> IO [event]))]
findProcessors moduleConfigs getProcessorFunc
    = moduleProcessors
    where
        moduleProcessorsM = map (\moduleConfig -> (moduleId moduleConfig, ioConstants moduleConfig, ioStateT moduleConfig, getProcessorFunc moduleConfig)) moduleConfigs
        moduleProcessorsJ = filter (\(_, _, _, processFuncM) -> isJust processFuncM) moduleProcessorsM
        moduleProcessors = map (\(id, ioConst, iostate, (Just processFunc)) -> (id, ioConst, iostate, processFunc)) moduleProcessorsJ

        
eventloopSteps :: (progstateT -> In -> (progstateT, [Out])) {-^ eventloop function -}
               -> TVar progstateT
               -> [In]
               -> IO [Out]
eventloopSteps eventloop progstateT inEvents
    =  sequencedSteps >>= (return.concat)
    where
        inEventSteps = map (eventloopStep eventloop progstateT) inEvents
        sequencedSteps = sequence inEventSteps
    
        
eventloopStep :: (progstateT -> In -> (progstateT, [Out])) {-^ eventloop function -}
              -> TVar progstateT
              -> In
              -> IO [Out]
eventloopStep eventloop progStateT inEvent
    = do
        progState <- readTVarIO progStateT
        let
            (progState', outEvents) = eventloop progState inEvent
        atomically $ writeTVar progStateT progState'
        return outEvents