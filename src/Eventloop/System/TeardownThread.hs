module Eventloop.System.TeardownThread
    ( startTeardowning
    ) where

import Control.Exception
import Control.Concurrent.STM

import Eventloop.Types.Exception
import Eventloop.Types.System


startTeardowning :: EventloopSystemConfiguration progstateT
                 -> IO ()
startTeardowning systemConfig
    = do
        sharedIO <- readTVarIO sharedIOStateT_
        sharedIO' <- teardownModules sharedConst sharedIO moduleConfigs_
        atomically $ writeTVar sharedIOStateT_ sharedIO'
    where
        sharedConst = sharedIOConstants systemConfig
        sharedIOStateT_ = sharedIOStateT systemConfig
        moduleConfigs_ = moduleConfigs systemConfig
        

teardownModules :: SharedIOConstants
                -> SharedIOState
                -> [EventloopModuleConfiguration]
                -> IO SharedIOState
teardownModules _ sharedIO [] = return sharedIO
teardownModules sharedConst sharedIO (moduleConfig:configs)
    = do
        sharedIO' <- teardownModule sharedConst sharedIO moduleConfig
        teardownModules sharedConst sharedIO' configs


teardownModule :: SharedIOConstants
               -> SharedIOState
               -> EventloopModuleConfiguration
               -> IO SharedIOState
teardownModule sharedConst sharedIO moduleConfig
    = case (teardownM moduleConfig) of
        Nothing         -> return (sharedIO)
        (Just teardown) -> handle
                ( \exception ->
                    throwIO (TeardownException moduleId_ exception)
                )
               ( do
                    ioState <- readTVarIO ioStateT_
                    teardown sharedConst sharedIO ioConst ioState
               )
    where
        moduleId_ = moduleId moduleConfig
        ioConst = ioConstants moduleConfig
        ioStateT_ = ioStateT moduleConfig