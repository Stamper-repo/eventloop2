module Eventloop.System.Processing where

import Control.Exception
import Control.Concurrent.ExceptionUtility
import Control.Concurrent.STM

import Eventloop.Types.Common
import Eventloop.Types.Exception
import Eventloop.Types.Events
import Eventloop.Types.System

{-
    To handle the pre-/postprocessing of in/out events
    Per module is needed: (moduleIdentifier, ioStateT, processFunc)
    Only modules with the appropriate processFunc are included if they exist
-}

processEventWithModule :: ProcessingDescription
                   -> SharedIOConstants
                   -> TVar SharedIOState
                   -> ( EventloopModuleIdentifier
                      , IOConstants
                      , TVar IOState
                      , (SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> event -> IO [event])
                      )
                   -> event
                   -> IO [event]
processEventWithModule processingDescription sharedConst sharedIOT (moduleId, ioConst, ioStateT, processFunc) event
    = handle
        ( \exception ->
            throwIO (ProcessingException processingDescription moduleId exception)
        )
        ( do
            processFunc sharedConst sharedIOT ioConst ioStateT event
        )


processEventsWithModules :: ProcessingDescription
                         -> SharedIOConstants
                         -> TVar SharedIOState
                         -> [( EventloopModuleIdentifier
                             , IOConstants
                             , TVar IOState
                             , (SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> event -> IO [event])
                             )]
                         -> [event] 
                         -> IO [event]
processEventsWithModules _ _ _ _ []
    = return []
processEventsWithModules _ _ _ [] events
    = return events
processEventsWithModules processingDescription sharedConst sharedIOT (moduleProcessor:mps) (event:events)
    = do
        generatedEvents <- processEventWithModule processingDescription sharedConst sharedIOT moduleProcessor event
        processedEvents <- processEventsWithModules processingDescription sharedConst sharedIOT mps generatedEvents
        restProcessedEvents <- processEventsWithModules processingDescription sharedConst sharedIOT (moduleProcessor:mps) events
        return (processedEvents ++ restProcessedEvents)       
                                  
                                  
processEvents :: ProcessingDescription
              -> EventloopSystemConfiguration progstateT
              -> [( EventloopModuleIdentifier
                  , IOConstants
                  , TVar IOState
                  , (SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> event -> IO [event])
                  )]
              -> [event] 
              -> IO [event]
processEvents processingDescription systemConfig moduleProcessors events
    = processEventsWithModules processingDescription sharedConst sharedIOT moduleProcessors events
    where
        sharedIOT = sharedIOStateT systemConfig
        sharedConst = sharedIOConstants systemConfig