module TestStdIn where

import Prelude

import Eventloop.EventloopCore
import Eventloop.DefaultConfiguration
import Eventloop.Types.EventTypes

import Eventloop.Module.StdIn
import Eventloop.Module.StdOut

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfig = defaultConfig { moduleConfigurations=[ defaultStdInModuleConfiguration
                                                       , defaultStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopConfiguration beginProgramState eventloop
                    
                    
eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [OutStdOut $ StdOutMessage "Char: ", OutStdIn $ StdInReceiveChar, OutStdOut $ StdOutMessage "line: ", OutStdIn $ StdInReceiveLine])
eventloop ps (InStdIn (StdInReceivedChar c)) = (ps, [OutStdOut $ StdOutMessage ("\n" ++ "Received Char: " ++ [c] ++ "\n")])
eventloop ps (InStdIn (StdInReceivedLine l)) = (ps, [OutStdOut $ StdOutMessage ("\n" ++ "Received Line: " ++ l ++ "\n"), Stop])
                    
start = startMainloop eventloopConfig