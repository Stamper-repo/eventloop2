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
threads. This is done by throwing exceptions to all workerthreads.
-}
startEventloopSystem :: EventloopSetupConfiguration progstateT
                     -> IO ()
startEventloopSystem setupConfig
    = do
        systemconfig <- setupEventloopSystemConfig setupConfig -- Setup
        let
            moduleConfigs_ = moduleConfigs systemconfig
            exceptions_ = exceptions systemconfig
            isStoppingM_ = isStoppingM systemconfig
        catch ( do
                    startInitializing systemconfig -- Initialization
                    let
                        retrieverThreadActions = threadActionsBasedOnModule systemconfig startRetrieving retrieverM moduleConfigs_
                        outRouterAction = startOutRouting systemconfig
                        senderThreadActions = threadActionsBasedOnModule systemconfig startSending senderConfigM moduleConfigs_
                    -- Spawn worker threads
                    mapM_ (spawnWorkerThread systemconfig registerRetrieverThread) retrieverThreadActions
                    spawnWorkerThread systemconfig registerOutRouterThread outRouterAction
                    mapM_ (spawnWorkerThread systemconfig registerSenderThread) senderThreadActions
                    startEventlooping systemconfig -- Eventlooping
              )
              ( \shutdownException -> do
                    swapMVar isStoppingM_ True -- If its already true, nothing happens, otherwise notify other threads the system thread is already shutting down
                    logException exceptions_ shutdownException

                    -- Stop the worker threads
                    workerThreads <- allWorkerThreads systemconfig
                    mapM_ throwShutdownExceptionToThread workerThreads
                    joinThreads workerThreads

                    startTeardowning systemconfig
                    startDisplayingExceptions systemconfig
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
threadActionsBasedOnModule systemconfig action getResourceFunc (moduleConfig:mcs)
    = case (getResourceFunc moduleConfig) of
        Nothing         -> otherThreadActions
        (Just resource) -> (action systemconfig (moduleConfig, resource)):otherThreadActions
    where
        otherThreadActions = threadActionsBasedOnModule systemconfig action getResourceFunc mcs

        
spawnWorkerThread :: EventloopSystemConfiguration progstateT
                  -> (EventloopSystemConfiguration progstateT -> Thread -> IO ())
                  -> IO ()
                  -> IO ()
spawnWorkerThread systemconfig logAction action
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
        logAction systemconfig thread
                                                    
    where
        exceptions_ = exceptions systemconfig
        systemTid = systemThreadId systemconfig
        isStoppingM_ = isStoppingM systemconfig
                    
 
registerRetrieverThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerRetrieverThread systemconfig thread
    = do
        retrieverThreads <- takeMVar retrieverThreadsM_
        putMVar retrieverThreadsM_ (retrieverThreads ++ [thread])
    where
        retrieverThreadsM_ = retrieverThreadsM systemconfig


registerOutRouterThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerOutRouterThread systemconfig thread
    = putMVar (outRouterThreadM systemconfig) thread


registerSenderThread :: EventloopSystemConfiguration progstateT
                        -> Thread
                        -> IO ()
registerSenderThread systemconfig thread
    = do
        senderThreads <- takeMVar senderThreadsM_
        putMVar senderThreadsM_ (senderThreads ++ [thread])
    where
        senderThreadsM_ = senderThreadsM systemconfig
        

throwShutdownExceptionToThread :: Thread -> IO ()
throwShutdownExceptionToThread thread
    = throwTo (getThreadId thread) ShuttingDownException
        
        
allWorkerThreads :: EventloopSystemConfiguration progstateT
                 -> IO [Thread]
allWorkerThreads systemconfig
    = do
        retrievers <- retrieverThreads systemconfig
        outRouter <- outRouterThread systemconfig
        senders <- senderThreads systemconfig
        return (retrievers ++ outRouter ++ senders)


retrieverThreads :: EventloopSystemConfiguration progstateT
                 -> IO [Thread]
retrieverThreads systemconfig
    = readMVar retrieverThreadsM_
    where
        retrieverThreadsM_ = retrieverThreadsM systemconfig


outRouterThread :: EventloopSystemConfiguration progstateT
                -> IO [Thread]
outRouterThread systemconfig
    = do
        hasNotOutRouterThread <- isEmptyMVar outRouterThreadM_
        case hasNotOutRouterThread of
                    True  -> return []
                    False -> do
                                outRouterThread <- readMVar outRouterThreadM_
                                return [outRouterThread]
    where
        outRouterThreadM_ = outRouterThreadM systemconfig


senderThreads :: EventloopSystemConfiguration progstateT
                 -> IO [Thread]
senderThreads systemconfig
    = readMVar senderThreadsM_
    where
        senderThreadsM_ = senderThreadsM systemconfig
