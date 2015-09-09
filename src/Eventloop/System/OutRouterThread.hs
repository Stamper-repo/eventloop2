module Eventloop.System.OutRouterThread where

import Control.Exception
import Control.Monad
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.OutRouter
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.Exception
import Eventloop.Types.System


startOutRouting :: EventloopSystemConfiguration progstateT
                -> IO ()
startOutRouting systemConfig
    = forever $ do
                    outEvent <- takeFromBlockingConcurrentQueue outEventQueue_
                    case outEvent of
                        Stop -> throwIO RequestShutdownException
                        _    -> outRouteOne moduleIdsSenderQueues outEvent
    where
        moduleIdsSenderQueues = outRoutes (moduleConfigs systemConfig)
        outEventQueue_ = outEventQueue (eventloopConfig systemConfig)
        

outRoutes :: [EventloopModuleConfiguration] -> [(EventloopModuleIdentifier, SenderEventQueue)]
outRoutes [] = []
outRoutes (moduleConfig:mcs) = case (senderConfigM moduleConfig) of
                                    Nothing                   -> outRoutes mcs
                                    (Just moduleSenderConfig) -> (moduleId moduleConfig, senderEventQueue moduleSenderConfig):(outRoutes mcs)
        
        
outRouteOne :: [(EventloopModuleIdentifier, SenderEventQueue)]
            -> Out
            -> IO ()
outRouteOne targetIdsSenderQueues outEvent
    = case targetSenderQueueM of
        Nothing                  -> 
            throwIO (NoOutRouteException outEvent)
        (Just targetSenderQueue) ->
            putInBlockingConcurrentQueue targetSenderQueue outEvent
    where
        targetModuleIdentifier = routeOutEvent outEvent
        targetSenderQueueM = lookup targetModuleIdentifier targetIdsSenderQueues
