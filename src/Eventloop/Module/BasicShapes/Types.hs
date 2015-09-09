module Eventloop.Module.BasicShapes.Types
    ( module Eventloop.Module.BasicShapes.Types
    , CT.CanvasId
    ) where

import qualified Eventloop.Module.Websocket.Canvas.Types as CT
import Eventloop.Utility.Vectors

type GraphicalNumeric = Float
type Translation = Point

type Width = GraphicalNumeric
type Height = GraphicalNumeric
type Dimensions = (Width, Height)

type Radius = GraphicalNumeric

type Red = GraphicalNumeric
type Green = GraphicalNumeric
type Blue = GraphicalNumeric
type Alpha = GraphicalNumeric
type Color = (Red, Green, Blue, Alpha)
type FillColor = Color
type StrokeColor = Color

type StrokeLineThickness = GraphicalNumeric

type UpperLeft = Point
type UpperRight = Point
type LowerLeft = Point
type LowerRight = Point

type AmountOfPoints = Int

type FontFamily = [Char]
type FontSize = GraphicalNumeric


data BasicShapesOut = DrawShapes CT.CanvasId [Shape]
                    deriving (Show, Eq)

data Shape = BaseShape { primitive :: Primitive
                       , strokeLineThickness :: StrokeLineThickness
                       , strokeColor :: StrokeColor
                       , rotationM :: (Maybe Rotation)
                       }
           | CompositeShape { shapes :: [Shape]
                            , translationM :: (Maybe Translation)
                            , rotationM :: (Maybe Rotation)
                            } -- ^Should contain atleast 1 shape
           deriving (Show, Eq)
           
data Primitive = Rectangle { translation :: Translation -- ^| Translation is the corner closes to origin. Visually in canvas, this is top left. In a Cartesian coördinate system, this is bottom left.
                           , dimensions :: Dimensions
                           , fillColor :: FillColor
                           } -- ^Translation is upperleftcorner
               | Circle { translation :: Translation -- ^| Translation is the centre of the circle
                        , radius :: Radius
                        , fillColor :: FillColor
                        } -- ^Translation is center
               | Polygon { amountOfPoints :: AmountOfPoints
                         , translation :: Translation -- ^| Translation is the the centre of the polygon
                         , radius :: Radius
                         , fillColor :: FillColor
                         } -- ^The first point of the polygon, always starts in the direction from the x-axis.(Towards x-infinity)
               | Text { text :: [Char] 
                      , fontFamily :: FontFamily
                      , fontSize :: FontSize
                      , translation :: Translation
                      , fillColor :: FillColor
                      } -- ^Translation is horizontally the center and vertically the top of the text, does not have a boundingbox due to technical limitations
               | Line { point1 :: Point
                      , point2 :: Point 
                      }
               | MultiLine { point1 :: Point
                           , point2 :: Point
                           , otherPoints :: [Point]
                           }
               deriving (Show, Eq)
           
           
data Rotation = Rotation RotatePoint Angle -- ^| Rotation is around a point on the canvas. May be the centre of the boundingbox (enclosing rectangle) or an arbitrary point. Angle is in degrees and counter-clockwise in the coördinate system(from the x-axis to the y-axis) and visually on canvas clock-wise.
            deriving (Show, Eq)

data RotatePoint = AroundCenter
                 | AroundPoint Point
                deriving (Show, Eq)
              
data BoundingBox = BoundingBox LowerLeft UpperLeft UpperRight LowerRight -- ^| The point indications are from the perspective of a regular Cartesian coördinate system.
                deriving (Show, Eq)
                         

               
