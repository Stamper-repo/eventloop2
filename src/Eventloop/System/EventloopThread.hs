module Eventloop.System.EventloopThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.ExceptionUtility
import Control.Concurrent.MVar
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
    = do
        putInBlockingConcurrentQueue inEventQueue_ Start
        forever $ do
          inEvent <- takeFromBlockingConcurrentQueue inEventQueue_ -- Take an In event
          processedInEvents <- processEvents "Preprocessing" systemConfig modulePreprocessors [inEvent] -- Preprocess it
          outEvents <- eventloopSteps eventloop progstateM_ processedInEvents  -- Eventloop over the preprocessed In events
          processedOutEvents <- processEvents "Postprocessing" systemConfig modulePostprocessors outEvents -- Postprocess the Out events
          putAllInBlockingConcurrentQueue outEventQueue_ processedOutEvents -- Send the processed Out events to the OutRouter
    where
        eventloopConfig_ = eventloopConfig systemConfig
        eventloop = eventloopFunc eventloopConfig_
        progstateM_ = progstateM eventloopConfig_
        inEventQueue_ = inEventQueue eventloopConfig_
        outEventQueue_ = outEventQueue eventloopConfig_
        moduleConfigurations_ = moduleConfigs systemConfig
        modulePreprocessors = findProcessors moduleConfigurations_ preprocessorM
        modulePostprocessors = findProcessors moduleConfigurations_ postprocessorM

        
findProcessors :: [EventloopModuleConfiguration]
               -> (EventloopModuleConfiguration -> Maybe (SharedIOState -> IOState -> event -> IO (SharedIOState, IOState, [event]))) -- Pre-/Postprocessor function
               -> [(EventloopModuleIdentifier, MVar IOState, (SharedIOState -> IOState -> event -> IO (SharedIOState, IOState, [event])))]
findProcessors moduleConfigs getProcessorFunc
    = moduleProcessors
    where
        moduleProcessorsM = map (\moduleConfig -> (moduleId moduleConfig, iostateM moduleConfig, getProcessorFunc moduleConfig)) moduleConfigs
        moduleProcessorsJ = filter (\(_, _, processFuncM) -> isJust processFuncM) moduleProcessorsM
        moduleProcessors = map (\(id, iostate, (Just processFunc)) -> (id, iostate, processFunc)) moduleProcessorsJ

        
eventloopSteps :: (progstateT -> In -> (progstateT, [Out])) {-| eventloop function -}
               -> MVar progstateT
               -> [In]
               -> IO [Out]
eventloopSteps eventloop progstateM inEvents
    =  sequencedSteps >>= (return.concat)
    where
        inEventSteps = map (eventloopStep eventloop progstateM) inEvents
        sequencedSteps = sequence inEventSteps
    
        
eventloopStep :: (progstateT -> In -> (progstateT, [Out])) {-| eventloop function -}
              -> MVar progstateT
              -> In
              -> IO [Out]
eventloopStep eventloop progstateM inEvent
    = catch
        ( uninterruptibleUpdateResourceAndCalculate
            ( takeMVar progstateM
            )
            ( \progstate' ->
                putMVar progstateM progstate'
            )
            ( \progstate ->
                return (eventloop progstate inEvent)
            )
        )
        ( \exception ->
            throwIO (EventloopException exception)
        )
      >>= return.snd