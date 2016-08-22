module TestCanvas where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import Eventloop.Module.Websocket.Canvas
import Eventloop.Module.StdOut

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupCanvasModuleConfiguration
                                                       , setupStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ SetupCanvas 1 1 (512, 512) (CSSPosition CSSFromDefault (CSSPercentage 0, CSSPercentage 0))
                          , OutCanvas $ CanvasOperations 1 [ DrawPath (10, 10) [Rectangle (10, 10) (100, 100)] (PathStroke 1 (CanvasColor (0, 0, 255, 255))) (NoPathFill)
                                                           , DrawText (CanvasText "hello world!" (Font "calibri" 48) AlignLeft) (100, 100) (TextStroke 1 (CanvasGradient (LinearGradient (0, 100) (200, 100)) [(0, (255, 0, 0, 255)), (1, (0, 255, 0, 255))])) NoTextFill
                                                           , DrawText (CanvasText "jMjMjMjMjjjjjjjggboeeeeeeeeeeee!" (Font "calibri" 48) AlignLeft) (0, 0) NoTextStroke (TextFill (CanvasGradient (LinearGradient (0, 100) (200, 100)) [(0, (255, 0, 0, 255)), (1, (0, 255, 0, 255))]))
                                                           , DrawPath (0, 0) [Rectangle (0, 0) (1, 1)] (NoPathStroke) (PathFill (CanvasColor (255, 0, 0, 125)))
                                                           , Frame
                                                           ]
                          , OutCanvas $ MeasureText (CanvasText "jMjMjM" (Font "calibri" 20) AlignLeft)
                          --, OutCanvas $ TeardownCanvas 1
                          ])
eventloop ps (InCanvas (MeasuredText text measurements)) = (ps, [ OutStdOut (StdOutMessage ("Measurements: " ++ (show measurements) ++ "\n"))
                                                                , OutCanvas $ CanvasOperations 1 [ DrawPath (50, 50) [Rectangle (50,50) measurements] (PathStroke 1 (CanvasColor (0, 255, 0, 255))) NoPathFill
                                                                                                 , DrawText (CanvasText "jMjMjM" (Font "calibri" 20) AlignLeft) (50, 50) (TextStroke 2 (CanvasColor (255,0,0,255))) (TextFill (CanvasColor (0, 0, 255, 255)))
                                                                                                 , Frame
                                                                                                 , Clear $ ClearRectangle (50, 50) (10, 10)
                                                                                                 , DrawPath (50, 50) [Rectangle (50, 50) (5, 5)] NoPathStroke (PathFill (CanvasColor (0, 255, 0, 255)))
                                                                                                 , Frame
                                                                                                 ]
                                                                , Stop
                                                                ])
                          
                          
start = startEventloopSystem eventloopConfiguration
