module Eventloop.System.InitializationThread
    ( startInitializing
    ) where

import Control.Exception
import Control.Concurrent.STM
import Data.List

import Eventloop.Types.Exception
import Eventloop.Types.System


startInitializing :: EventloopSystemConfiguration progstateT
                  -> IO (EventloopSystemConfiguration progstateT)
startInitializing systemConfig
    = do
        sharedIO <- readTVarIO sharedIOT_
        (sharedConst', sharedIO', moduleConfigs_') <- initializeModules sharedConst sharedIO moduleConfigs_
        atomically $ writeTVar sharedIOT_ sharedIO'
        return systemConfig{moduleConfigs = moduleConfigs_', sharedIOConstants = sharedConst'}
    where
        sharedConst = sharedIOConstants systemConfig
        sharedIOT_ = sharedIOStateT systemConfig
        moduleConfigs_ = reverse $ moduleConfigs systemConfig


initializeModules :: SharedIOConstants
                  -> SharedIOState
                  -> [EventloopModuleConfiguration]
                  -> IO (SharedIOConstants, SharedIOState, [EventloopModuleConfiguration])
initializeModules sharedConst sharedIO [] = return (sharedConst, sharedIO, [])
initializeModules sharedConst sharedIO (moduleConfig:configs)
    = do
        (sharedConst', sharedIO', moduleConfig') <- initializeModule sharedConst sharedIO moduleConfig
        (sharedConst'', sharedIO'', configs') <- initializeModules sharedConst' sharedIO' configs
        return (sharedConst'', sharedIO'', moduleConfig':configs')


initializeModule :: SharedIOConstants
                 -> SharedIOState
                 -> EventloopModuleConfiguration
                 -> IO (SharedIOConstants, SharedIOState, EventloopModuleConfiguration)
initializeModule sharedConst sharedIO moduleConfig
    = case (initializerM moduleConfig) of
        Nothing            -> return (sharedConst, sharedIO, moduleConfig)
        (Just initializer) -> handle
            ( \exception ->
                throwIO (InitializationException moduleId_ exception)
            )
            ( do
                ioState <- readTVarIO ioStateT_
                (sharedConst', sharedIO', ioConst', ioState') <- initializer sharedConst sharedIO
                atomically $ writeTVar ioStateT_ ioState'
                return (sharedConst', sharedIO', moduleConfig {ioConstants = ioConst'})
            )
    where
        moduleId_ = moduleId moduleConfig
        ioConst = ioConstants moduleConfig
        ioStateT_ = ioStateT moduleConfig