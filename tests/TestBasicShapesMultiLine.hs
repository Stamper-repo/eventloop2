module TestBasicShapes where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes
import Eventloop.Utility.Vectors

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ C.setupCanvasModuleConfiguration
                                                                   , setupBasicShapesModuleConfiguration
                                                                   ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ C.SetupCanvas 1 1 (512, 512) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                          , OutBasicShapes $ DrawShapes 1 shapes
                          --, OutCanvas $ TeardownCanvas 1
                          , Stop
                          ])
                  where
                    shapes = [ Rectangle (Point (25, 25)) (100, 10) (255, 0, 0, 255) 1 (0, 0, 0, 255) (Just $ Rotation AroundCenter 45)
                             , Circle (Point (100, 100)) 25 (0, 255, 0, 255) 1 (0, 0, 0, 255) Nothing
                             , Line (Point (300, 0)) (Point (300, 40)) 1 (255, 0, 0, 255) Nothing
                             , MultiLine [Point (400,0), Point (400, 30), Point (425, 15), Point (425, 50), Point (400, 50)] 4 (255, 0, 0, 255) Nothing
                             , MultiLine [Point (350,15), Point (450, 15), Point (450, 30)] 3 (0, 255, 0, 255) Nothing
                             --, BaseShape (Line (Point (50, 300)) (Point (50, 330))) 1 (255, 0, 0, 255) (Just $ Rotation AroundCenter 0)
                             , Rectangle (Point (25, 25)) (100, 10) (255, 0, 0, 255) 1 (0, 0, 0, 255) (Just $ Rotation AroundCenter 45)
                             , Circle (Point (100, 100)) 25 (0, 255, 0, 255) 1 (0, 0, 0, 255) Nothing
                             ]
                          
                          
start = startEventloopSystem eventloopConfiguration
