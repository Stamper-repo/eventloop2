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
    = Drawn StatefulBB
    | Modified StatefulBB
    | Removed StatefulBB
    | NoOp

data StatefulGraphic = Stateful NamedId ZIndex Shape
                        deriving (Show, Eq)

data StatefulBB = StatefulBB StatefulGraphic BoundingBox
                deriving (Show, Eq)

type GraphicsState = [StatefulBB]
type GraphicsStates = [(CanvasId, GraphicsState)]


instance ToBoundingBox StatefulGraphic where
    toBoundingBox (Stateful _ _ shape) = toBoundingBox shape

instance Overlaps StatefulGraphic


instance ToBoundingBox StatefulBB where
    toBoundingBox (StatefulBB _ bb) = bb

instance Overlaps StatefulBB



class NoDimChange a where
    noDimChange :: a -> a -> Bool
{-
instance NoDimChange Shape where
    noDimChange
    TODO for Modified -> Drawn optimalization
-}
