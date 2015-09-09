module Eventloop.System.TeardownThread
    ( startTeardowning
    ) where

import Control.Exception
import Control.Concurrent.MVar

import Eventloop.System.ThreadActions
import Eventloop.Types.Exception
import Eventloop.Types.System


startTeardowning :: EventloopSystemConfiguration progstateT
                 -> IO ()
startTeardowning systemConfig
    = mapM_ (teardownModule sharedIOStateM_) moduleConfigs_
    where
        sharedIOStateM_ = sharedIOStateM systemConfig
        moduleConfigs_ = moduleConfigs systemConfig
        

teardownModule :: MVar SharedIOState 
                 -> EventloopModuleConfiguration
                 -> IO ()
teardownModule sharedIOStateM_ moduleConfig
    = case (teardownM moduleConfig) of
        Nothing            -> return ()
        (Just teardown) ->
            teardownIOState sharedIOStateM_ iostateM_
                ( \exception ->
                    throwIO (TeardownException moduleId_ exception)
                )
                teardown
    where
        moduleId_ = moduleId moduleConfig
        iostateM_ = iostateM moduleConfig