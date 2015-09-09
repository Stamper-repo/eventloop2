module Eventloop.System.ThreadActions where

import Control.Exception
import Control.Monad
import Control.Concurrent.ExceptionUtility
import Control.Concurrent.MVar

import Eventloop.Types.System

withSharedIOStateAndIOState :: MVar SharedIOState
                            -> MVar IOState
                            -> (SomeException -> IO ())
                            -> (SharedIOState -> IOState -> IO (SharedIOState, IOState))
                            -> IO ()
withSharedIOStateAndIOState sharedIOStateM_ iostateM_ onException updateFunc
    = catch (
          void $ uninterruptibleUpdateResource
              ( do
                  sharedIOState <- takeMVar sharedIOStateM_
                  iostate <- takeMVar iostateM_
                  return (sharedIOState, iostate)
              )
              ( \(sharedIOState', iostate') -> do
                  putMVar sharedIOStateM_ sharedIOState'
                  putMVar iostateM_ iostate'
              )
              ( \(sharedIOState, iostate) ->
                  updateFunc sharedIOState iostate
              )
      )
      ( \exception ->
          onException exception
      )
   
   
initializeIOState :: MVar SharedIOState
                  -> MVar IOState
                  -> (SomeException -> IO ())
                  -> (SharedIOState -> IO (SharedIOState, IOState))
                  -> IO ()
initializeIOState sharedIOStateM_ iostateM_ onException updateFunc
    = catch (
          void $ uninterruptibleUpdateResource
              ( do
                  sharedIOState <- takeMVar sharedIOStateM_
                  iostate <- takeMVar iostateM_
                  return (sharedIOState, iostate)
              )
              ( \(sharedIOState', iostate') -> do
                  putMVar sharedIOStateM_ sharedIOState'
                  putMVar iostateM_ iostate'
              )
              ( \(sharedIOState, iostate) ->
                  updateFunc sharedIOState
              )
      )
      ( \exception ->
          onException exception
      )
        
        
teardownIOState :: MVar SharedIOState
                -> MVar IOState
                -> (SomeException -> IO ())
                -> (SharedIOState -> IOState -> IO SharedIOState)
                -> IO ()
teardownIOState sharedIOStateM_ iostateM_ onException updateFunc
    = catch (
          void $ uninterruptibleUpdateResource
              ( do
                  sharedIOState <- takeMVar sharedIOStateM_
                  iostate <- takeMVar iostateM_
                  return (sharedIOState, iostate)
              )
              ( \(sharedIOState', _) -> do
                  putMVar sharedIOStateM_ sharedIOState'
                  putMVar iostateM_ NoState
              )
              ( \(sharedIOState, iostate) -> do
                    sharedIOState' <- updateFunc sharedIOState iostate
                    return (sharedIOState', undefined)
              )
      )
      ( \exception ->
          onException exception
      )