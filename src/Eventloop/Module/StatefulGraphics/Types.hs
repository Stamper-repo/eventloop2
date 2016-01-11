module Eventloop.Module.StatefulGraphics.Types
    ( module Eventloop.Module.Websocket.Canvas.Types
    , module Eventloop.Module.StatefulGraphics.Types
    ) where

import Eventloop.Types.Common
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.Websocket.Canvas.Types (CanvasId, ZIndex)

data StatefulGraphicsOut
    = Draw StatefulGraphic
    | Remove NamedId
    deriving (Eq, Show)

data GraphicPerformed
    = Drawn StatefulGraphic
    | Modified StatefulGraphic
    | Removed StatefulGraphic
    | NoOp

type StatefulGraphic = (NamedId, ZIndex, Shape)

type GraphicsState = [StatefulGraphic]
type GraphicsStates = [(CanvasId, GraphicsState)]

