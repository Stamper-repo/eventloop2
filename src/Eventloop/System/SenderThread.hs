module Eventloop.System.SenderThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

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
        case outEvent of
            Stop -> do
                        sendOne moduleId_ sharedConst sharedIOT ioConst ioStateT_ sender_ Stop
                        throwIO RequestShutdownException
            _    -> sendOne moduleId_ sharedConst sharedIOT ioConst ioStateT_ sender_ outEvent
    where
        moduleId_ = moduleId moduleConfig
        sharedConst = sharedIOConstants systemConfig
        sharedIOT = sharedIOStateT systemConfig
        ioConst = ioConstants moduleConfig
        ioStateT_ = ioStateT moduleConfig
        sender_ = sender moduleSenderConfig
        senderEventQueue_ = senderEventQueue moduleSenderConfig

                
sendOne :: EventloopModuleIdentifier
        -> SharedIOConstants
        -> TVar SharedIOState
        -> IOConstants
        -> TVar IOState
        -> EventSender
        -> Out
        -> IO ()
sendOne moduleId sharedConst sharedIOT ioConst ioStateT sender outEvent
    = handle ( \exception ->
                -- Wrap the exception if it isn't a ShuttingDownException
                case (fromException exception) of
                    (Just ShuttingDownException) -> throwIO ShuttingDownException
                    _                            -> throwIO (SendingException moduleId outEvent exception)
            )
            ( sender sharedConst sharedIOT ioConst ioStateT outEvent
            )