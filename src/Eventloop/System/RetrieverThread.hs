module Eventloop.System.RetrieverThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.System.ThreadActions
import Eventloop.Types.Common
import Eventloop.Types.Exception
import Eventloop.Types.System

startRetrieving :: EventloopSystemConfiguration progstateT
                -> (EventloopModuleConfiguration, EventRetriever)
                -> IO ()
startRetrieving systemConfig (moduleConfig, retriever)
    = forever (retrieveOne moduleId_ sharedIOStateM_ iostateM_ retriever inEventQueue_)
    where
        moduleId_ = moduleId moduleConfig
        eventloopConfiguration = eventloopConfig systemConfig
        sharedIOStateM_ = sharedIOStateM systemConfig
        inEventQueue_ = inEventQueue eventloopConfiguration
        iostateM_ = iostateM moduleConfig


retrieveOne :: EventloopModuleIdentifier ->
               MVar SharedIOState ->
               MVar IOState ->
               EventRetriever ->
               InEventQueue ->
               IO ()
retrieveOne moduleId sharedIOStateM_ iostateM retriever inEventQueue
    = withSharedIOStateAndIOState sharedIOStateM_ iostateM
        ( \exception -> 
            throwIO (RetrievingException moduleId exception)
        )
        ( \sharedIOState iostate -> do
            (sharedIOState', iostate', inEvents) <- retriever sharedIOState iostate
            putAllInBlockingConcurrentQueue inEventQueue inEvents
            return (sharedIOState', iostate')
        )