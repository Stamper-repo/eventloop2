module Eventloop.Core where

import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.Thread

import Eventloop.System.DisplayExceptionThread
import Eventloop.System.EventloopThread
import Eventloop.System.InitializationThread
import Eventloop.System.OutRouterThread
import Eventloop.System.RetrieverThread
import Eventloop.System.SenderThread
import Eventloop.System.Setup
import Eventloop.System.TeardownThread
import Eventloop.Types.Exception
import Eventloop.Types.System

{- | Starts the entire system. First the setup phase is handled to setup the different
concurrent resources. This is followed by the initialization phase where all modules are initialised.
Than, the different worker threads are spawned and finally the system thread will go to work as the eventloop thread.

Shutting down is handled centrally through the system thread (main thread).
If any of the threads(including the system thread) receive an exception, only the first exception is thrown to the system
thread which will try to shutdown immediately. This exception is logged by the system thread.
All other exceptions are logged by their respective threads. The system thread will than shutdown the worker
threads. 
-}
startEventloopSystem :: EventloopSetupConfiguration progstateT
                     -> IO ()
startEventloopSystem setupConfig
    = do
        systemConfig <- setupEventloopSystemConfig setupConfig -- Setup
        let
            moduleConfigs_ = moduleConfigs systemConfig
            exceptions_ = exceptions systemConfig
            isStoppingM_ = isStoppingM systemConfig
        catch ( do
                    startInitializing systemConfig -- Initialization
                    let
                        retrieverThreadActions = threadActionsBasedOnModule systemConfig startRetrieving retrieverM moduleConfigs_
                        outRouterAction = startOutRouting systemConfig
                        senderThreadActions = threadActionsBasedOnModule systemConfig startSending senderConfigM moduleConfigs_
                    -- Spawn worker threads
                    mapM_ (spawnWorkerThread systemConfig registerRetrieverThread) retrieverThreadActions
                    spawnWorkerThread systemConfig registerOutRouterThread outRouterAction
                    mapM_ (spawnWorkerThread systemConfig registerSenderThread) senderThreadActions
                    startEventlooping systemConfig -- Eventlooping
              )
              ( \shutdownException -> do
                    swapMVar isStoppingM_ True -- If its already true, nothing happens, otherwise notify other threads the system thread is already shutting down
                    logException exceptions_ shutdownException
                    
                    workerThreads <- allWorkerThreads systemConfig
                    mapM_ throwShutdownExceptionToThread workerThreads
                    joinThreads workerThreads
                    
                    startTeardowning systemConfig
                    startDisplayingExceptions systemConfig
              )

{- |
Utility function in order to create the different thread actions in the system.
Assumed is that the action requires the system configuration, the module configuration and some resource
that may be available in the module configuration.
-}
threadActionsBasedOnModule :: EventloopSystemConfiguration progstateT
                           -> (EventloopSystemConfiguration progstateT -> (EventloopModuleConfiguration, resource) -> IO ())
                           -> (EventloopModuleConfiguration -> Maybe resource)
                           -> [EventloopModuleConfiguration]
                           -> [IO ()]
threadActionsBasedOnModule _ _ _ [] = []
threadActionsBasedOnModule systemConfig action getResourceFunc (moduleConfig:mcs)
    = case (getResourceFunc moduleConfig) of
        Nothing         -> otherThreadActions
        (Just resource) -> (action systemConfig (moduleConfig, resource)):otherThreadActions
    where
        otherThreadActions = threadActionsBasedOnModule systemConfig action getResourceFunc mcs

        
spawnWorkerThread :: EventloopSystemConfiguration progstateT
                  -> (EventloopSystemConfiguration progstateT -> Thread -> IO ())
                  -> IO ()
                  -> IO ()
spawnWorkerThread systemConfig logAction action
    = do
        thread <- forkThread $ do
            catch action
                ( \exception ->
                        case exception of
                            ShuttingDownException ->
                                return ()
                            _                     -> do
                                isStopping <- takeMVar isStoppingM_
                                case isStopping of
                                    True -> logException exceptions_ exception
                                    False -> throwTo systemTid exception
                                putMVar isStoppingM_ True
                    )
        logAction systemConfig thread
                                                    
    where
        exceptions_ = exceptions systemConfig
        systemTid = systemThreadId systemConfig
        isStoppingM_ = isStoppingM systemConfig
                    
 
registerRetrieverThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerRetrieverThread systemConfig thread
    = do
        retrieverThreads <- takeMVar retrieverThreadsM_
        putMVar retrieverThreadsM_ (retrieverThreads ++ [thread])
    where
        retrieverThreadsM_ = retrieverThreadsM systemConfig

registerOutRouterThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerOutRouterThread systemConfig thread
    = putMVar (outRouterThreadM systemConfig) thread
        
registerSenderThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerSenderThread systemConfig thread
    = do
        senderThreads <- takeMVar senderThreadsM_
        putMVar senderThreadsM_ (senderThreads ++ [thread])
    where
        senderThreadsM_ = senderThreadsM systemConfig
        

throwShutdownExceptionToThread :: Thread -> IO ()
throwShutdownExceptionToThread thread
    = throwTo (getThreadId thread) ShuttingDownException
        
        
allWorkerThreads :: EventloopSystemConfiguration progstateT
                 -> IO [Thread]
allWorkerThreads systemConfig
    = do
        retrieverThreads <- readMVar retrieverThreadsM_
        senderThreads <- readMVar senderThreadsM_
        hasNotOutRouterThread <- isEmptyMVar outRouterThreadM_
        case hasNotOutRouterThread of
            True                -> return (retrieverThreads ++ senderThreads)
            False -> do
                        outRouterThread <- readMVar outRouterThreadM_
                        return (retrieverThreads ++ [outRouterThread] ++ senderThreads)
    where
        retrieverThreadsM_ = retrieverThreadsM systemConfig
        outRouterThreadM_ = outRouterThreadM systemConfig
        senderThreadsM_ = senderThreadsM systemConfig
