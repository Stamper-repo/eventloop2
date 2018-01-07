module TestMouse where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.System
import Eventloop.Types.Events

import Eventloop.Module.Websocket.Mouse
import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes
import Eventloop.Module.StdOut

import Eventloop.Utility.Vectors

data ProgramState = ProgramState
                  deriving (Eq, Show)
                  
beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupMouseModuleConfiguration
                                                       , C.setupCanvasModuleConfiguration
                                                       , setupStdOutModuleConfiguration
                                                       , setupBasicShapesModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ C.SetupCanvas 1 1 (512, 512) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                          , OutCanvas $ C.SetupCanvas 2 2 (512, 512) (C.CSSPosition C.CSSFromDefault (C.CSSPercentage 0, C.CSSPercentage 0))
                          , OutBasicShapes $ DrawShapes 1 [Rectangle (Point (0,0)) (512, 512) (255, 0,0,255) 1 (0,0,0,0) Nothing]
                          ])
eventloop ps (InMouse event) = (ps, [OutStdOut (StdOutMessage (show event ++ "\n"))])

start = startEventloopSystem eventloopConfiguration