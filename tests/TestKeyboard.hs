module TestKeyboard where

import Prelude
import Eventloop.EventloopCore
import Eventloop.DefaultConfiguration
import Eventloop.Types.EventTypes

import Eventloop.Module.Websocket.Keyboard
import Eventloop.Module.StdOut

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfig = defaultConfig { moduleConfigurations=[ defaultKeyboardModuleConfiguration
                                                       , defaultStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopConfiguration beginProgramState eventloop

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps (InKeyboard (Key "T")) = (ps, [OutStdOut (StdOutMessage $ "Final Pressed: " ++ "T\n"), Stop])
eventloop ps (InKeyboard (Key c)) = (ps, [OutStdOut (StdOutMessage $ "Pressed: " ++ c ++ "\n")])
eventloop ps _ = (ps, [])

start = startMainloop eventloopConfig