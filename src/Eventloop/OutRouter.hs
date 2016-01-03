module Eventloop.OutRouter where

import Eventloop.Types.Events
import Eventloop.Types.System

import Eventloop.Module.File
import Eventloop.Module.Timer
import Eventloop.Module.StatefulGraphics
import Eventloop.Module.StdIn
import Eventloop.Module.StdOut
import Eventloop.Module.Websocket.Canvas

routeOutEvent :: OutEventRouter
routeOutEvent out = case out of
                            (OutFile _) -> fileModuleIdentifier
                            (OutTimer _) -> timerModuleIdentifier
                            (OutStdOut _) -> stdOutModuleIdentifier
                            (OutStdIn _) -> stdInModuleIdentifier
                            (OutCanvas _) -> canvasModuleIdentifier
                            (OutStatefulGraphics _ _) -> statefulGraphicsModuleIdentifier
                            _ -> error ("Could not find route for out event: " ++ show out)