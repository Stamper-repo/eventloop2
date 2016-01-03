module Eventloop.Types.Events where

import Eventloop.Module.Websocket.Keyboard.Types
import Eventloop.Module.Websocket.Mouse.Types
import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.DrawTrees.Types
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.File.Types
import Eventloop.Module.StatefulGraphics.Types
import Eventloop.Module.StdIn.Types
import Eventloop.Module.StdOut.Types
import Eventloop.Module.Timer.Types
import Eventloop.Module.Graphs.Types


data In = Start
        | InKeyboard Keyboard
        | InMouse MouseIn
        | InFile FileIn
        | InTimer TimerIn
        | InStdIn StdInIn
        | InCanvas CanvasIn
        | InGraphs GraphsIn
        deriving (Eq, Show)


data Out = OutFile FileOut
         | OutTimer TimerOut
         | OutStdOut StdOutOut
         | OutStdIn StdInOut
         | OutCanvas CanvasOut
         | OutBasicShapes BasicShapesOut
         | OutDrawTrees DrawTreesOut
         | OutGraphs GraphsOut
         | OutStatefulGraphics CanvasId [StatefulGraphicsOut]
         | Stop
         deriving (Eq, Show)