module Eventloop.System.OutRouterThread where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.OutRouter
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.Exception
import Eventloop.Types.System

{- | Grab an outEvent from the outEventQueue and route it to the correct module sender if any.
If there isn't one, throw a NoOutRouteException. The router will continue until it:
- Comes across a Stop outEvent: Raises a RequestShutdownException
- Raises an exception
- Receives an exception (Only possibility is ShutdownException)
In all cases, a Stop outEvent is sent to all module senders.
-}
startOutRouting :: EventloopSystemConfiguration progstateT
                -> IO ()
startOutRouting systemConfig
    = catch (forever $ do
                outEvent <- takeFromBlockingConcurrentQueue outEventQueue_
                case outEvent of
                    Stop -> throwIO RequestShutdownException
                    _    -> outRouteOne moduleIdsSenderQueues outEvent
            )
            (\exception -> do
                outRouteBroadcastStop moduleIdsSenderQueues
                throwIO (exception :: SomeException)
            )
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


outRouteBroadcastStop :: [(EventloopModuleIdentifier, SenderEventQueue)]
                      -> IO ()
outRouteBroadcastStop []
    = return ()
outRouteBroadcastStop targetIdsSenderQueues
    = mapM_ broadcastAction (map snd targetIdsSenderQueues)
    where
        broadcastAction targetSenderQueue =
            putInBlockingConcurrentQueue targetSenderQueue Stop