{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.BasicShapes.Types
    ( module Eventloop.Module.BasicShapes.Types
    , CanvasId
    , Alignment(..)
    ) where

import Eventloop.Module.Websocket.Canvas.Types (CanvasId, Alignment(..))
import Eventloop.Utility.Vectors

import GHC.Generics (Generic)
import Control.DeepSeq


type GraphicalNumeric = Float
type Position = Point

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

type FillThickness = GraphicalNumeric
type StrokeLineThickness = GraphicalNumeric

type UpperLeft = Point
type UpperRight = Point
type LowerLeft = Point
type LowerRight = Point

type NumberOfPoints = Int

type FontFamily = [Char]
type FontSize = GraphicalNumeric


data BasicShapesOut = DrawShapes CanvasId [Shape]
                    deriving (Show, Eq, Generic, NFData)

data Shape = CompositeShape { shapes :: [Shape]
                            , positionM :: Maybe Position
                            , rotationM :: Maybe Rotation
                            } -- ^Should contain atleast 1 shape. Rotation before Position
           | Rectangle { position :: Position
                       , dimensions :: Dimensions
                       , fillColor :: FillColor
                       , strokeLineThickness :: StrokeLineThickness
                       , strokeColor :: StrokeColor
                       , rotationM :: Maybe Rotation
                       } -- ^| Position is upperleftcorner. Position is the corner closes to origin. Visually in canvas, this is top left. In a Cartesian coördinate system, this is bottom left.
           | Circle { position :: Position
                    , radius :: Radius
                    , fillColor :: FillColor
                    , strokeLineThickness :: StrokeLineThickness
                    , strokeColor :: StrokeColor
                    , rotationM :: Maybe Rotation
                    } -- ^| Position is center
           | RegularPolygon { position :: Position
                     , numberOfPoints :: NumberOfPoints
                     , radius :: Radius
                     , fillColor :: FillColor
                     , strokeLineThickness :: StrokeLineThickness
                     , strokeColor :: StrokeColor
                     , rotationM :: Maybe Rotation
                     } -- ^The first point of the regular polygon, always starts in the direction from the x-axis.(Towards x-infinity). Position is the the centre of the regular polygon
           | Text { text :: [Char]
                  , fontFamily :: FontFamily
                  , fontSize :: FontSize
                  , position :: Position
                  , alignment :: Alignment
                  , fillColor :: FillColor
                  , strokeLineThickness :: StrokeLineThickness
                  , strokeColor :: StrokeColor
                  , rotationM :: Maybe Rotation
                  }
           | Line { point1 :: Point
                  , point2 :: Point
                  , strokeLineThickness :: StrokeLineThickness
                  , strokeColor :: StrokeColor
                  , rotationM :: Maybe Rotation
                  }
           | MultiLine { points :: [Point] -- ^| Can contain any number of points
                       , strokeLineThickness :: StrokeLineThickness
                       , strokeColor :: StrokeColor
                       , rotationM :: Maybe Rotation
                       }
           | FilledMultiLine { points :: [Point]
                             , fillWidth :: FillThickness
                             , fillColor :: FillColor
                             , strokeLineThickness :: StrokeLineThickness
                             , strokeColor :: StrokeColor
                             , rotationM :: Maybe Rotation
                             }
           | Polygon { points :: [Point] -- ^| Closes shape automatically with fill. Do not forget that the fill encloses the entire path. This means that half of the stroke is overwritten by the fill.
                     , fillColor :: FillColor
                     , strokeLineThickness :: StrokeLineThickness
                     , strokeColor :: StrokeColor
                     , rotationM :: Maybe Rotation
                     }
           deriving (Show, Eq, Generic, NFData)
           
           
data Rotation = Rotation RotatePoint Angle -- ^| Rotation is around a point on the canvas. May be the centre of the boundingbox (enclosing rectangle) or an arbitrary point. Angle is in degrees and counter-clockwise in the coördinate system(from the x-axis to the y-axis) and visually on canvas clock-wise.
            deriving (Show, Eq, Generic, NFData)

data RotatePoint = AroundCenter
                 | AroundPoint Point
                deriving (Show, Eq, Generic, NFData)
              
data BoundingBox = BoundingBox LowerLeft UpperLeft UpperRight LowerRight -- ^| The point indications are from the perspective of a regular Cartesian coördinate system.
                deriving (Show, Eq)
                         

               
