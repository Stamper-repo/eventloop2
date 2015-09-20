module Eventloop.System.SenderThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.System.ThreadActions
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.Exception
import Eventloop.Types.System

startSending :: EventloopSystemConfiguration progstateT 
             -> (EventloopModuleConfiguration, EventloopModuleSenderConfiguration)
             -> IO ()
startSending systemConfig (moduleConfig, moduleSenderConfig)
    = forever $ do
                    outEvent <- takeFromBlockingConcurrentQueue senderEventQueue_
                    sendOne moduleId_ sharedIOStateM_ iostateM_ sender_ outEvent
    where
        moduleId_ = moduleId moduleConfig
        sharedIOStateM_ = sharedIOStateM systemConfig
        iostateM_ = iostateM moduleConfig
        sender_ = sender moduleSenderConfig
        senderEventQueue_ = senderEventQueue moduleSenderConfig

                
sendOne :: EventloopModuleIdentifier
        -> MVar SharedIOState
        -> MVar IOState
        -> EventSender
        -> Out
        -> IO ()
sendOne moduleId sharedIOStateM_ iostateM sender outEvent
    = withSharedIOStateAndIOState sharedIOStateM_ iostateM
        ( \exception ->
            -- Wrap the exception if it isn't a ShuttingDownException
            case (fromException exception) of
                (Just ShuttingDownException) -> throwIO ShuttingDownException
                _                            -> throwIO (SendingException moduleId outEvent exception)
        )
        ( \sharedIOState iostate ->
            sender sharedIOState iostate outEvent
        )