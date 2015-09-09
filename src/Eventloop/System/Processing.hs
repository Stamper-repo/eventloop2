module Eventloop.System.Processing where

import Control.Exception
import Control.Concurrent.ExceptionUtility
import Control.Concurrent.MVar

import Eventloop.Types.Common
import Eventloop.Types.Exception
import Eventloop.Types.Events
import Eventloop.Types.System

{-
    To handle the pre-/postprocessing of in/out events
    Per module is needed: (moduleIdentifier, iostateM, processFunc)
    Only modules with the appropriate processFunc are included if they exist
-}

processEventWithModule :: ProcessingDescription
                   -> MVar SharedIOState
                   -> ( EventloopModuleIdentifier
                      , MVar IOState 
                      , (SharedIOState -> IOState -> event -> IO (SharedIOState, IOState, [event]))
                      )
                   -> event
                   -> IO [event]
processEventWithModule processingDescription sharedIOM (moduleId, iostateM, processFunc) event
    = catch
        ( uninterruptibleUpdateResourceAndCalculate
            ( do
                sharedIO <- takeMVar sharedIOM
                iostate <- takeMVar iostateM
                return (sharedIO, iostate)
            )
            ( \(sharedIO', iostate') -> do
                putMVar sharedIOM sharedIO'
                putMVar iostateM iostate'
            )
            ( \(sharedIO, iostate) -> do
                (sharedIO', iostate', events') <- processFunc sharedIO iostate event
                return ((sharedIO', iostate'), events')
            )
        )
        ( \exception ->
            throwIO (ProcessingException processingDescription moduleId exception)
        )
      >>= return.snd


processEventsWithModules :: ProcessingDescription
                         -> MVar SharedIOState 
                         -> [( EventloopModuleIdentifier
                             , MVar IOState 
                             , (SharedIOState -> IOState -> event -> IO (SharedIOState, IOState, [event]))
                             )]
                         -> [event] 
                         -> IO [event]
processEventsWithModules _ _ _ []
    = return []
processEventsWithModules _ _ [] events
    = return events
processEventsWithModules processingDescription sharedIOM (moduleProcessor:mps) (event:events) 
    = do
        generatedEvents <- processEventWithModule processingDescription sharedIOM moduleProcessor event
        processedEvents <- processEventsWithModules processingDescription sharedIOM mps generatedEvents
        restProcessedEvents <- processEventsWithModules processingDescription sharedIOM (moduleProcessor:mps) events
        return (processedEvents ++ restProcessedEvents)       
                                  
                                  
processEvents :: ProcessingDescription
              -> EventloopSystemConfiguration progstateT
              -> [( EventloopModuleIdentifier
                  , MVar IOState 
                  , (SharedIOState -> IOState -> event -> IO (SharedIOState, IOState, [event]))
                  )]
              -> [event] 
              -> IO [event]
processEvents processingDescription systemConfig moduleProcessors events
    = processEventsWithModules processingDescription sharedIOM moduleProcessors events
    where
        sharedIOM = sharedIOStateM systemConfig