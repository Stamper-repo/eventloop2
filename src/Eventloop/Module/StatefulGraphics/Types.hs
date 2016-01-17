module Eventloop.Module.StatefulGraphics.Types
    ( module Eventloop.Module.Websocket.Canvas.Types
    , module Eventloop.Module.StatefulGraphics.Types
    ) where

import Eventloop.Types.Common
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.BasicShapes.Classes
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

data StatefulGraphic = Stateful NamedId ZIndex Shape
                        deriving (Show, Eq)

type GraphicsState = [StatefulGraphic]
type GraphicsStates = [(CanvasId, GraphicsState)]


instance ToBoundingBox StatefulGraphic where
    toBoundingBox (Stateful _ _ shape) = toBoundingBox shape

instance Overlaps StatefulGraphic



class NoDimChange a where
    noDimChange :: a -> a -> Bool
{-
instance NoDimChange Shape where
    noDimChange
    TODO for Modified -> Drawn optimalization
-}
