{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
module Eventloop.Utility.Vectors where

import GHC.Generics (Generic)
import Control.DeepSeq

type Angle = Float -- ^In degrees
type Radians = Float
type Length = Float

type X = Float
type Y = Float
type Offset = (X, Y)

data PolarCoord = PolarCoord (Length, Radians)
                deriving (Show, Eq)
                
data Point = Point (X, Y)
            deriving (Show, Eq, Generic, NFData)


class Coord a where
    x :: a -> X
    y :: a -> Y

instance Coord Point where
    x (Point (x_, _)) = x_
    y (Point (_, y_)) = y_

instance Coord PolarCoord where
    x = x.toPoint
    y = y.toPoint


class ExtremaCoord a where
    xMin :: a -> X
    xMax :: a -> X
    yMin :: a -> Y
    yMax :: a -> Y

instance ExtremaCoord [Point] where
    xMin points = minimum $ map x points
    xMax points = maximum $ map x points
    yMin points = minimum $ map y points
    yMax points = maximum $ map y points


degreesToRadians :: Angle -> Radians
degreesToRadians d = (pi / 180) * d


radiansToDegrees :: Radians -> Angle
radiansToDegrees rads = (180 / pi) * rads


lengthToPoint :: Point -> Length
lengthToPoint = lengthBetweenPoints originPoint


lengthBetweenPoints :: Point -> Point -> Length
lengthBetweenPoints p1 p2 = sqrt (x'^2 + y'^2)
                        where
                            (x', y') = differenceBetweenPoints p1 p2
                       

differenceBetweenPoints :: Point -> Point -> (X, Y)
differenceBetweenPoints (Point (x1, y1)) (Point (x2, y2)) = (x2 - x1, y2 - y1)


averagePoint :: [Point] -> Point
averagePoint points
    = average
        where
            total = foldl (|+|) originPoint points
            average = total |\ (toInteger (length points))


-- | Returns the vector perpendicular on the given vector between the 2 points. Always has positive y and vector length 1; y is inverted in canvas
downPerpendicular :: Point -> Point -> Point
downPerpendicular p1@(Point (x1, y1)) p2@(Point (x2, y2))
    | y2 > y1   = Point ((-1) * sign * (abs yv) / size, (abs xv) / size)
    | otherwise = Point (       sign * (abs yv) / size, (abs xv) / size)
    where
        (xv, yv) = differenceBetweenPoints p1 p2
        size     = lengthBetweenPoints p1 p2
        sign     = case xv of
                    0 -> (-1)
                    _ -> xv / (abs xv)


-- | Returns the vector perpendicular on the given vector between the 2 points. Always has negative y and vector length 1; y is inverted in canvas
upPerpendicular :: Point -> Point -> Point
upPerpendicular p1 p2 = negateVector $ downPerpendicular p1 p2


followVector :: Float -> Point -> Point -> Point
followVector distance followP startP
    = (followP |* fraction) |+| startP
    where
        fraction = distance / size
        size     = lengthBetweenPoints followP originPoint


originPoint = Point (0,0)

class Translate a where
    translate :: Point -> a -> a


class (Coord a) => Vector2D a where
    (|+|) :: a -> a -> a
    (|-|) :: a -> a -> a
    (|\)  :: (Real b) => a -> b -> a
    (|*)  :: (Real b) => a -> b -> a
    negateVector :: a -> a

instance Vector2D PolarCoord where
    pc1 |+| pc2 = toPolarCoord $ (toPoint pc1) |+| (toPoint pc2)
    pc1 |-| pc2 = toPolarCoord $ (toPoint pc1) |-| (toPoint pc2)
    (PolarCoord (l, a)) |\ scalar
        = PolarCoord (fromRational (l' / scalar'), a)
        where
            l' = toRational l
            scalar' = toRational scalar
    (PolarCoord (l, a)) |* scalar
        = PolarCoord (fromRational (l' * scalar'), a)
        where
            l' = toRational l
            scalar' = toRational scalar
    negateVector pc1 = rotateLeftAround (Point (0,0)) 180 pc1
    
instance Vector2D Point where
    (Point (x1, y1)) |+| (Point (x2, y2))
        = Point (x1 + x2, y1 + y2)

    (Point (x1, y1)) |-| (Point (x2, y2))
        = Point (x1 - x2, y1 - y2)

    (Point (x1, y1)) |\  scalar
        = Point (fromRational x', fromRational y')
        where
            x' = toRational x1 / toRational scalar
            y' = toRational y1 / toRational scalar

    (Point (x1, y1)) |*  scalar
        = Point (fromRational x', fromRational y')
        where
            x' = toRational x1 * toRational scalar
            y' = toRational y1 * toRational scalar

    negateVector (Point (x, y))
        = Point (-x, -y)

    
class ToPoint a where
    toPoint :: a -> Point
    
instance ToPoint PolarCoord where
    toPoint (PolarCoord (len, rads)) = Point (len * (cos rads), len * (sin rads))
    
    
class ToPolarCoord a where
    toPolarCoord :: a -> PolarCoord
    
instance ToPolarCoord Point where
    toPolarCoord (Point (x, y)) | x == 0 && y == 0 = PolarCoord (0.0, 0.0)
                                | x == 0 && y > 0  = PolarCoord (y, 0.5 * pi)
                                | x == 0 && y < 0  = PolarCoord (y, 1.5 * pi)
                                | x > 0  && y == 0 = PolarCoord (x, 0.0 * pi)
                                | x < 0  && y == 0 = PolarCoord (x, 1.0 * pi)
                                | x > 0 && y > 0   = PolarCoord (len, 0.0 * pi + localRads)
                                | x < 0 && y > 0   = PolarCoord (len, 1.0 * pi - localRads)
                                | x < 0 && y < 0   = PolarCoord (len, 1.0 * pi + localRads)
                                | x > 0 && y < 0   = PolarCoord (len, 2.0 * pi - localRads)
                                 where
                                    x' = abs x
                                    y' = abs y
                                    localRads = asin (y' / len)
                                    len = lengthToPoint (Point (x, y))
                            


class RotateLeftAround a where
    rotateLeftAround :: Point -> Angle -> a -> a
 
instance RotateLeftAround PolarCoord where
    rotateLeftAround rotatePoint aDeg = toPolarCoord.(rotateLeftAround rotatePoint aDeg).toPoint

 
instance RotateLeftAround Point where 
    rotateLeftAround rotatePoint aDeg p = p'' |+| rotatePoint
                                        where
                                            p' = p |-| rotatePoint
                                            pc'@(PolarCoord (len', rads')) = toPolarCoord p'
                                            aRads = degreesToRadians aDeg
                                            pc'' = PolarCoord (len', rads' + aRads)
                                            p'' = toPoint pc''