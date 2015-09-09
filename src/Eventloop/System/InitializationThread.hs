module Eventloop.System.InitializationThread
    ( startInitializing
    ) where

import Control.Exception
import Control.Concurrent.MVar

import Eventloop.System.ThreadActions
import Eventloop.Types.Exception
import Eventloop.Types.System


startInitializing :: EventloopSystemConfiguration progstateT
                  -> IO ()
startInitializing systemConfig
    = mapM_ (initializeModule sharedIOStateM_) moduleConfigs_
    where
        sharedIOStateM_ = sharedIOStateM systemConfig
        moduleConfigs_ = moduleConfigs systemConfig
        

initializeModule :: MVar SharedIOState 
                 -> EventloopModuleConfiguration
                 -> IO ()
initializeModule sharedIOStateM_ moduleConfig
    = case (initializerM moduleConfig) of
        Nothing            -> return ()
        (Just initializer) -> do
            initializeIOState sharedIOStateM_ iostateM_
                ( \exception ->
                    throwIO (InitializationException moduleId_ exception)
                )
                initializer
    where
        moduleId_ = moduleId moduleConfig
        iostateM_ = iostateM moduleConfig