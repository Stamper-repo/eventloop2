module TestParseTrees where

import Prelude

import Eventloop.Core
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.DefaultConfiguration
import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes
import Eventloop.Module.DrawTrees
import Eventloop.Utility.Trees.GeneralTree

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupDrawTreesModuleConfiguration
                                                                   , setupBasicShapesModuleConfiguration
                                                                   , C.setupCanvasModuleConfiguration
                                                                   ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop



eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop state Start = (state, [ OutCanvas $ C.SetupCanvas 1 1 (1024, 1024) (C.CSSPosition C.CSSFromDefault (C.CSSPercentage 0, C.CSSPercentage 0))
                                , OutDrawTrees $ DrawTrees 1 [ generalizeTree rbExampleTree
                                                             , generalizeTree roseExampleTree
                                                             ]
                                , Stop
                                ]
                        )

start = startEventloopSystem eventloopConfiguration
