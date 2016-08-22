module Main where

import Prelude

import Eventloop.Core
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.DefaultConfiguration

import Eventloop.Module.Timer
import Eventloop.Module.StdOut

data ProgramState = ProgramState Int
                    deriving (Eq, Show)
                    
beginProgramState = ProgramState 0

eventloopConfiguration = defaultConfig { setupModuleConfigurations = [ setupTimerModuleConfiguration
                                                                     , setupStdOutModuleConfiguration
                                                                     ]}
                where                                        
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [OutTimer $ SetIntervalTimer "IT1" 100000, OutTimer $ SetTimer "IT2" 2000000, OutTimer $ SetTimer "IT3" 3000000])
eventloop ps (InTimer (Tick "IT2")) = (ps, [OutTimer $ UnsetTimer "IT1"])
eventloop (ProgramState i) (InTimer (Tick "IT3")) = (ProgramState i, [OutStdOut $ StdOutMessage ("Count total: " ++ (show i) ++ "\n"), Stop])
eventloop (ProgramState i) (InTimer (Tick id)) = (ProgramState (i + 1), [OutStdOut $ StdOutMessage ("Tick: " ++ id ++ "\n")])

start = startEventloopSystem eventloopConfiguration
