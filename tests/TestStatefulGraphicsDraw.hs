module TestStatefulGraphicsDraw where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Vectors

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes
import Eventloop.Module.StatefulGraphics
import Eventloop.Module.StdOut
import Eventloop.Module.Timer


data ProgramState = ProgramState Int
                  deriving (Eq, Show)

beginProgramState = ProgramState 0

eventloopConfiguration = EventloopSetupConfiguration { beginProgstate = beginProgramState
                                                     , eventloopF = eventloop
                                                     , setupModuleConfigurations = [ setupStatefulGraphicsModuleConfiguration
                                                                                   , setupBasicShapesModuleConfiguration
                                                                                   , C.setupCanvasModuleConfiguration
                                                                                   , setupTimerModuleConfiguration
                                                                                   , setupStdOutModuleConfiguration
                                                                                   ]
                                                     }

canvasId :: C.CanvasId
canvasId = 1

rectShape id z pos col dims = Stateful id  z (Rectangle (Point pos) dims col 0 (0, 0, 0, 0) Nothing)

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ C.SetupCanvas canvasId 1 (1024, 1024) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                          , OutStatefulGraphics canvasId $ map Draw [ rectShape "12" 12 (140, 60) (0, 0, 255, 255) (10, 50)
                                                                    , rectShape "11" 11 (50, 50) (255, 0, 0, 255) (120, 20)

                                                                    , rectShape "20" 20 (200, 50) (255, 0, 0, 255) (10, 10)

                                                                    , rectShape "30" 30 (250, 50) (255, 0, 0, 255) (50, 50)

                                                                    , rectShape "40" 40 (350, 50) (255, 0, 0, 255) (75, 75)

                                                                    , rectShape "51" 51 (450, 50) (255, 0, 0, 255) (75, 75)

                                                                    , rectShape "60" 60 (575, 75) (0, 255, 0, 255) (25, 25)

                                                                    , rectShape "71" 71 (675, 75) (0, 255, 0, 255) (25, 25)

                                                                    , rectShape "81" 81 (750, 75) (0, 255, 0, 255) (50, 25)
                                                                    ]
                          , OutTimer $ SetIntervalTimer "1" 1000000
                          ])

eventloop (ProgramState i) (InTimer (Tick "1"))
    = ( ProgramState (i + 1)
      , out
      )
    where
        out = case i of
                0 -> [ OutStdOut $ StdOutMessage "Test 1: Partial overlap behind + ripple. New rectangle should be behind middle and middle behind right\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "10" 10 (40, 60) (0, 255, 0, 255) (50, 50)]
                     ]
                1 -> [ OutStdOut $ StdOutMessage "Test 2: Rectangles aren't touching another. Existing shouldn't be redrawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "21" 21 (220, 50) (0, 255, 0, 255) (10, 10)]
                     ]
                2 -> [ OutStdOut $ StdOutMessage "Test 3: Partial overlap front. New is before existing. Only new should be drawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "31" 31 (240, 70) (0, 255, 0, 255) (20, 20)]
                     ]
                3 -> [ OutStdOut $ StdOutMessage "Test 4: New front, contained by Old. Only new should be drawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "41" 41 (375, 75) (0, 255, 0, 255) (25, 25)]
                     ]
                4 -> [ OutStdOut $ StdOutMessage "Test 5: New behind, contained by Old. Both are drawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "50" 50 (475, 75) (0, 255, 0, 255) (25, 25)]
                     ]
                5 -> [ OutStdOut $ StdOutMessage "Test 6 New front, contains Old. New is drawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "61" 61 (550, 50) (255, 0, 0, 255) (75, 75)]
                     ]
                6 -> [ OutStdOut $ StdOutMessage "Test 7: New behind, contains Old. Both are drawn\n\n"
                     , OutStatefulGraphics canvasId [Draw $ rectShape "70" 70 (650, 50) (255, 0, 0, 255) (75, 75)]
                     ]
                7 -> [ OutStdOut $ StdOutMessage "Test 8: New behind, old modified. Only new and new old are drawn, old old are removed\n\n"
                     , OutStatefulGraphics canvasId [ Draw $ rectShape "80" 80 (750, 75) (255, 0, 0, 255) (25, 50)
                                                    , Draw $ rectShape "81" 81 (775, 75) (0, 255, 0, 255) (50, 25)
                                                    ]
                     ]
                8 -> [ Stop
                     ]


start = startEventloopSystem eventloopConfiguration
