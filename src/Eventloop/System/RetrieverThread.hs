module Eventloop.System.RetrieverThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.Types.Common
import Eventloop.Types.Exception
import Eventloop.Types.System

startRetrieving :: EventloopSystemConfiguration progstateT
                -> (EventloopModuleConfiguration, EventRetriever)
                -> IO ()
startRetrieving systemConfig (moduleConfig, retriever)
    = forever (retrieveOne moduleId_ sharedConst sharedIOStateT_ ioConst ioStateT_ retriever inEventQueue_)
    where
        moduleId_ = moduleId moduleConfig
        eventloopConfiguration = eventloopConfig systemConfig
        sharedConst = sharedIOConstants systemConfig
        sharedIOStateT_ = sharedIOStateT systemConfig
        inEventQueue_ = inEventQueue eventloopConfiguration
        ioConst = ioConstants moduleConfig
        ioStateT_ = ioStateT moduleConfig


retrieveOne :: EventloopModuleIdentifier ->
               SharedIOConstants ->
               TVar SharedIOState ->
               IOConstants ->
               TVar IOState ->
               EventRetriever ->
               InEventQueue ->
               IO ()
retrieveOne moduleId sharedConst sharedIOT ioConst iostateT retriever inEventQueue
    = handle ( \exception ->
                -- Wrap the exception if it isn't a ShuttingDownException
                case (fromException exception) of
                    (Just ShuttingDownException) -> throwIO ShuttingDownException
                    _                            -> throwIO (RetrievingException moduleId exception)
            )
            ( do
                inEvents <- retriever sharedConst sharedIOT ioConst iostateT
                putAllInBlockingConcurrentQueue inEventQueue inEvents
            )