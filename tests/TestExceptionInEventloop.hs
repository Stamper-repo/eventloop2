module TestFile where

import Prelude

import Eventloop.Core
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.DefaultConfiguration
import Eventloop.Module.File
import Eventloop.Module.StdOut
import System.IO

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations = [ setupFileModuleConfiguration
                                                                     , setupStdOutModuleConfiguration
                                                                     ]
                                }
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop


eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop state Start = error "This is someone who created an exception in his eventloop. That sucks!"

start = startEventloopSystem eventloopConfiguration
