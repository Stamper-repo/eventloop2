module Eventloop.System.TeardownThread
    ( startTeardowning
    ) where

import Control.Exception
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.STM

import Eventloop.Types.Exception
import Eventloop.Types.System


startTeardowning :: EventloopSystemConfiguration progstateT
                 -> IO ()
startTeardowning systemConfig
    = do
        sharedIO <- readTVarIO sharedIOStateT_
        sharedIO' <- teardownModules sharedConst sharedIO systemConfig moduleConfigs_
        atomically $ writeTVar sharedIOStateT_ sharedIO'
    where
        sharedConst = sharedIOConstants systemConfig
        sharedIOStateT_ = sharedIOStateT systemConfig
        moduleConfigs_ = moduleConfigs systemConfig
        

teardownModules :: SharedIOConstants
                -> SharedIOState
                -> EventloopSystemConfiguration progstateT
                -> [EventloopModuleConfiguration]
                -> IO SharedIOState
teardownModules _ sharedIO _ [] = return sharedIO
teardownModules sharedConst sharedIO systemConfig (moduleConfig:configs)
    = do
        sharedIO' <- teardownModule sharedConst sharedIO systemConfig moduleConfig
        teardownModules sharedConst sharedIO' systemConfig configs


teardownModule :: SharedIOConstants
               -> SharedIOState
               -> EventloopSystemConfiguration progstateT
               -> EventloopModuleConfiguration
               -> IO SharedIOState
teardownModule sharedConst sharedIO systemConfig moduleConfig
    = case (teardownM moduleConfig) of
        Nothing         -> return (sharedIO)
        (Just teardown) -> handle
                ( \exception -> do
                    logException (exceptions systemConfig) (toException $ TeardownException moduleId_ exception)
                    return sharedIO
                )
               ( do
                    ioState <- readTVarIO ioStateT_
                    teardown sharedConst sharedIO ioConst ioState
               )
    where
        moduleId_ = moduleId moduleConfig
        ioConst = ioConstants moduleConfig
        ioStateT_ = ioStateT moduleConfig