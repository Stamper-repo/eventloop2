module Eventloop.Module.StatefulGraphics.Types where

import Eventloop.Types.Common
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.Websocket.Canvas.Types

data StatefulGraphicsOut
    = Draw StatefulGraphic
    | Remove NamedId
    deriving (Eq, Show)

data GraphicPerformed
    = Drawn StatefulGraphic
    | Modified StatefulGraphic
    | Removed StatefulGraphic
    | NoOp

type StatefulGraphic = (NamedId, Shape)

type GraphicsState = [StatefulGraphic]
type GraphicsStates = [(CanvasId, GraphicsState)]

