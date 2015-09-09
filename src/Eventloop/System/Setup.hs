module Eventloop.System.Setup
    ( setupEventloopSystemConfig
    ) where

import Control.Concurrent
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.MVar
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

import Eventloop.DefaultConfiguration
import Eventloop.Types.System


setupEventloopSystemConfig :: EventloopSetupConfiguration progstateT 
                           -> IO (EventloopSystemConfiguration progstateT)
setupEventloopSystemConfig setupConfig
    = do
        eventloopConfig_ <- setupEventloopConfiguration setupConfig
        moduleConfigurations_ <- mapM setupEventloopModuleConfig (setupModuleConfigurations setupConfig)
        sharedIOState_ <- setupSharedIOState
        sharedIOStateM_ <- newMVar sharedIOState_
        systemThreadId_ <- myThreadId
        retrieverThreadsM_ <- newMVar []
        outRouterThreadM_ <- newEmptyMVar
        senderThreadsM_ <- newMVar []
        exceptionCollection <- createExceptionCollection
        isStoppingM_ <- newMVar False
        
        return ( EventloopSystemConfiguration
                    eventloopConfig_
                    moduleConfigurations_
                    sharedIOStateM_
                    systemThreadId_
                    retrieverThreadsM_
                    outRouterThreadM_
                    senderThreadsM_
                    exceptionCollection
                    isStoppingM_
               )
        

setupEventloopConfiguration :: EventloopSetupConfiguration progstateT
                            -> IO (EventloopConfiguration progstateT)
setupEventloopConfiguration setupConfig
    = do
        progstateM_ <- newMVar (beginProgstate setupConfig)
        inEventQueue_ <- createBlockingConcurrentQueue
        outEventQueue_ <- createBlockingConcurrentQueue
        
        return ( EventloopConfiguration
                    progstateM_
                    (eventloopF setupConfig)
                    inEventQueue_
                    outEventQueue_
               )
        
        
setupEventloopModuleConfig :: EventloopSetupModuleConfiguration
                           -> IO EventloopModuleConfiguration
setupEventloopModuleConfig setupModuleConfig
    = do
        iostateM <- newMVar NoState
        senderConfig <- setupEventloopModuleSenderConfiguration (eventSenderF setupModuleConfig)
        
        return ( EventloopModuleConfiguration
                    (moduleIdentifier setupModuleConfig)
                    iostateM
                    (initializerF setupModuleConfig)
                    (eventRetrieverF setupModuleConfig)
                    (preprocessorF setupModuleConfig)
                    (postprocessorF setupModuleConfig)
                    senderConfig
                    (teardownF setupModuleConfig)
               )
    where
        moduleId_ = moduleIdentifier setupModuleConfig
        


setupEventloopModuleSenderConfiguration :: Maybe EventSender 
                                        -> IO (Maybe EventloopModuleSenderConfiguration)
setupEventloopModuleSenderConfiguration Nothing = return Nothing
setupEventloopModuleSenderConfiguration (Just eventSender_)
    = do
        senderEventQueue_ <- createBlockingConcurrentQueue
        
        return (Just (EventloopModuleSenderConfiguration eventSender_ senderEventQueue_))