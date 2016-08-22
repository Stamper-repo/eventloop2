module TestGraphs where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.BasicShapes as B
import qualified Eventloop.Module.Websocket.Mouse as M
import qualified Eventloop.Module.Websocket.Keyboard as K
import Eventloop.Module.Graphs
import Eventloop.Module.StdOut

data ProgramState = ProgramState Graph
                  deriving (Eq, Show)

beginProgramState = ProgramState beginGraph

beginGraph = Graph allNodes allEdges Directed Weighted
           where
            allNodes = [ ('a', (50, 50), Red)
                       , ('b', (150, 50), Blue)
                       , ('c', (200, 200), Orange)
                       ]
            allEdges = [ ('a', 'b', Green, 5, Thick)
                       , ('c', 'b', Orange, 3, Thin)
                       , ('c', 'a', Purple, 2, Thin)
                       ]

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupGraphsModuleConfiguration
                                                                   , B.setupBasicShapesModuleConfiguration
                                                                   , C.setupCanvasModuleConfiguration
                                                                   , M.setupMouseModuleConfiguration
                                                                   , K.setupKeyboardModuleConfiguration
                                                                   , setupStdOutModuleConfiguration
                                                                   ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

instructions = [ "Instructions"
               , "Hello! g!"
               ]
             
                    
eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop (ProgramState graph) Start = (ProgramState graph, [ OutGraphs SetupGraphs
                                                            , OutGraphs $ DrawGraph graph
                                                            , OutGraphs $ Instructions instructions
                                                            ])
                                                            
eventloop ps (InGraphs (Key "s")) = (ps, [Stop])

eventloop ps (InGraphs in') = (ps, [OutStdOut $ StdOutMessage (show in' ++ "\n")])
eventloop ps _ = (ps, [])

start = startEventloopSystem eventloopConfiguration
