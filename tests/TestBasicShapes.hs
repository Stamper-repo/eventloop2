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

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupBasicShapesModuleConfiguration
                                                                   , C.setupCanvasModuleConfiguration
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
                             , Rectangle (Point (25, 25)) (100, 10) (255, 0, 0, 255) 1 (0, 0, 0, 255) Nothing
                             , Text "Hello World!" "New Times Roman" 24 (Point (250, 250)) AlignLeft (125, 125, 125, 255) 1 (255, 0, 0, 255) Nothing
                             , RegularPolygon (Point (350, 350)) 3 50 (0, 0, 255, 255) 1 (255, 0, 0, 255) Nothing
                             , RegularPolygon (Point (425, 425)) 4 50 (0, 0, 255, 255) 1 (255, 0, 0, 255) Nothing
                             , CompositeShape [ RegularPolygon (Point (0, 0)) 5 50 (0, 0, 255, 255) 1 (0, 0, 0, 255) Nothing
                                              , Line (Point (0, 0)) (Point (50, 0)) 1 (255, 0, 0, 255) Nothing
                                              ]
                                              (Just (Point (250, 400)))
                                              Nothing --(Just $ Rotation AroundCenter 90)
                             , Line (Point (300, 0)) (Point (300, 40)) 1 (255, 0, 0, 255) Nothing
                             , MultiLine [Point (400,0), Point (400, 30), Point (425, 15)] 1 (255, 0, 0, 255) Nothing
                             , MultiLine [] 1 (255, 0, 0, 255) Nothing
                             , MultiLine [Point (430, 15)] 1 (255, 0, 0, 255) Nothing
                             , Line (Point (50, 300)) (Point (50, 330)) 1 (255, 0, 0, 255) (Just $ Rotation AroundCenter 0)
                             , Line (Point (50, 310)) (Point (50, 340)) 1 (0, 255, 0, 255) (Just $ Rotation AroundCenter 45)
                             , Line (Point (50, 320)) (Point (50, 350)) 1 (0, 0, 255, 255) (Just $ Rotation AroundCenter 90)
                             , Line (Point (50, 330)) (Point (50, 360)) 1 (255, 0, 0, 255) (Just $ Rotation AroundCenter 135)
                             , Line (Point (50, 340)) (Point (50, 370)) 1 (0, 255, 0, 255) (Just $ Rotation AroundCenter 180)
                             , Line (Point (50, 350)) (Point (50, 380)) 1 (0, 0, 255, 255) (Just $ Rotation AroundCenter 225)
                             , Line (Point (50, 360)) (Point (50, 390)) 1 (255, 0, 0, 255) (Just $ Rotation AroundCenter 270)
                             , Line (Point (50, 370)) (Point (50, 400)) 1 (0, 255, 0, 255) (Just $ Rotation AroundCenter 315)
                             , Line (Point (50, 380)) (Point (50, 410)) 1 (0, 0, 255, 255) (Just $ Rotation AroundCenter 360)
                             , Polygon [Point (475, 475), Point (475, 500), Point (470, 500), Point (480, 512), Point (490, 500), Point (485, 500), Point (485, 475)] (0, 255, 0, 125) 3 (255, 0, 0, 255) Nothing
                             ]
                          
                          
start = startEventloopSystem eventloopConfiguration
