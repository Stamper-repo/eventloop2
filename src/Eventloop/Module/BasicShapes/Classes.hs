module Eventloop.Module.BasicShapes.Classes where

import Control.Concurrent.MVar
import Data.Maybe

import Eventloop.Utility.Vectors
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.BasicShapes.MeasureTextHack
import qualified Eventloop.Module.Websocket.Canvas.Types as CT


{-
The center of a boundingbox is not the center of an element
    Rectangle - Intersection of two halves of adjoining sides
    Circle    - Centre point
    RegularPolygon   - Centre point
    Text      - See boundingbox to rectangle
    Line      - Halfway down the line
    MultiLine - See boundingbox
Split into points to calc boundingbox
    Rectangle - Split into 4 corners
    Circle    - Top, left, right and bottom points on the circle
    RegularPolygon   - Split into the regular polygon points
    Text      - Split into 4 corners of bbox
    Line      - Split into the two points
    MultiLine - Split into the different points with stroke
    Polygon   - Split into the different points with stroke
-}

{-
Bugs:
- BoundingBox of circle fails. Cannot use 'rotate points on circle' method.
- Difficult rotation? (Stacked composite shapes)
-}

data GeometricPrimitive = Points [Point]
                        | CircleArea Point Radius


instance RotateLeftAround GeometricPrimitive where
    rotateLeftAround p angle (Points points)
        = Points $ map (rotateLeftAround p angle) points
    rotateLeftAround p angle (CircleArea p' r)
        = CircleArea (rotateLeftAround p angle p') r


opOnBoundingBox :: (Point -> Point) -> BoundingBox -> BoundingBox
opOnBoundingBox op (BoundingBox p1 p2 p3 p4) = BoundingBox (op p1)
                                                           (op p2)
                                                           (op p3)
                                                           (op p4)
 

instance ExtremaCoord BoundingBox where
    xMin (BoundingBox ll _ _ _) = x ll
    xMax (BoundingBox _ _ ur _) = x ur
    yMin (BoundingBox ll _ _ _) = y ll
    yMax (BoundingBox _ _ ur _) = y ur


instance ExtremaCoord GeometricPrimitive where
    xMin (Points points) = xMin points
    xMin (CircleArea (Point (x, y)) r) = x - r

    xMax (Points points) = xMax points
    xMax (CircleArea (Point (x, y)) r) = x + r

    yMin (Points points) = yMin points
    yMin (CircleArea (Point (x, y)) r) = y - r

    yMax (Points points) = yMax points
    yMax (CircleArea (Point (x, y)) r) = y + r


instance RotateLeftAround BoundingBox where
    rotateLeftAround p angle bbox = opOnBoundingBox (rotateLeftAround p angle) bbox



allRegularPolygonPoints :: NumberOfPoints -> Point -> Radius -> [Point]
allRegularPolygonPoints n centralPoint r | n < 1 = error "A regular polygon with 0 or more sides doesn't exist!"
                                  | otherwise = [centralPoint |+| (toPoint (PolarCoord (r, angle)))  |angle <- anglesRads]
                                where
                                    anglePart = 360 / (fromIntegral n)
                                    startAngle = 0
                                    anglesDeg = filter (< 360) [startAngle, startAngle + anglePart..360]
                                    anglesRads = map degreesToRadians anglesDeg


boundingBoxFromPrimitives :: [GeometricPrimitive] -> BoundingBox
boundingBoxFromPrimitives primitives
    = BoundingBox (Point (xMin_, yMin_)) (Point (xMin_, yMax_)) (Point (xMax_, yMax_)) (Point (xMax_, yMin_))
    where
        xMin_ = minimum $ map xMin primitives
        xMax_ = maximum $ map xMax primitives
        yMin_ = minimum $ map yMin primitives
        yMax_ = maximum $ map yMax primitives


normalizeBBox :: BoundingBox -> BoundingBox
normalizeBBox (BoundingBox p1 p2 p3 p4)
    = boundingBoxFromPrimitives [Points [p1, p2, p3, p4]]


roundPoint :: Point -> CT.ScreenPoint
roundPoint (Point (x, y)) = (round x, round y)


roundColor :: Color -> CT.ScreenColor
roundColor (r, b, g, a) = (round r, round b, round g, a)


instance Translate Shape where
    translate p c@(CompositeShape {positionM=Nothing})
        = c {positionM = (Just p)}
    translate p c@(CompositeShape {positionM=(Just p1)})
        = c {positionM = (Just $ p1 |+| p)}
    translate p r@(Rectangle {position=trans})
        = r {position = trans |+| p}
    translate p c@(Circle {position=trans})
        = c {position = trans |+| p}
    translate p po@(RegularPolygon {position=trans})
        = po {position = trans |+| p}
    translate p t@(Text {position=trans})
        = t {position = trans |+| p}
    translate pTrans l@(Line {point1=p1, point2=p2})
        = l {point1 = (p1 |+| pTrans), point2 = (p2 |+| pTrans)}
    translate pTrans ml@(MultiLine {points=points})
        = ml {points = (map ((|+|) pTrans) points)}
    translate pTrans a@(Polygon {points=points})
        = a {points = (map ((|+|) pTrans) points)}

instance Translate GeometricPrimitive where
    translate p (Points points) = Points (map (|+| p) points)
    translate p (CircleArea p' r) = CircleArea (p |+| p') r


class ToPrimitives a where
    toPrimitives :: a -> [GeometricPrimitive]

instance ToPrimitives BoundingBox where
    toPrimitives (BoundingBox ll ul ur lr) = [Points [ll, ul, ur, lr]]

instance ToPrimitives Shape where
    toPrimitives (CompositeShape shapes positionM Nothing)
        | isJust positionM = map (translate (fromJust positionM)) primitives
        | otherwise           = primitives
        where
            primitives = concat $ map toPrimitives shapes
    toPrimitives (Rectangle {position=(Point (x, y)), dimensions=(w, h), strokeLineThickness=thick, rotationM=Nothing})
        = [ Points [ Point (x - hthick, y - hthick)
                   , Point (x - hthick, y + h + hthick)
                   , Point (x + w + hthick, y + h + hthick)
                   , Point (x + w + hthick, y - hthick)
                   ]
          ]
        where
            hthick = 0.5 * thick
    toPrimitives (Circle {position=p, radius=r, strokeLineThickness=thick, rotationM=Nothing})
        = [CircleArea p (r + 0.5 * thick)]
    toPrimitives (RegularPolygon {numberOfPoints=a, position=p, radius=r, strokeLineThickness=thick, rotationM=Nothing})
        | a > 2  = toPrimitives (MultiLine points thick undefined Nothing)
        | a == 2 = toPrimitives (Line p1 p2 thick undefined Nothing)
        | a == 1 = [Points (take 1 points)]
        | a == 0 = [Points []]
        where
             points = allRegularPolygonPoints a p r
             (p1:p2:ops) = points
    toPrimitives text@(Text {position=(Point (x,y)), alignment=align, rotationM=Nothing})
        = [ Points $ case align of
            CT.AlignLeft   -> [ Point (x, y)
                              , Point (x, y + height)
                              , Point (x + width, y)
                              , Point (x + width, y + height)
                              ]
            CT.AlignCenter -> [ Point (x - hwidth, y - hheight)
                              , Point (x - hwidth, y + hheight)
                              , Point (x + hwidth, y - hheight)
                              , Point (x + hwidth, y + hheight)
                              ]
            CT.AlignRight  -> [ Point (x, y)
                              , Point (x, y + height)
                              , Point (x - width, y)
                              , Point (x - width, y + height)
                              ]
          ]
        where
            canvasText = toCanvasText text
            (width_, height_) = useMeasureText canvasText
            width = fromIntegral width_
            hwidth = width * 0.5
            height = fromIntegral height_
            hheight = height * 0.5

    toPrimitives (Line {point1=p1, point2=p2, strokeLineThickness=thick, rotationM=Nothing})
        = [ Points [ followVector (0.5 * thick) upPerpVector p1
                   , followVector (0.5 * thick) upPerpVector p2
                   , followVector (0.5 * thick) downPerpVector p1
                   , followVector (0.5 * thick) downPerpVector p2
                   ]
          ]
        where
            upPerpVector = upPerpendicular p1 p2
            downPerpVector = negateVector upPerpVector
    toPrimitives (MultiLine {points=points, strokeLineThickness=thick, rotationM=Nothing})
        = concat $ map toPrimitives lines
        where
            tailPoints = drop 1 points
            linePoints = zip points tailPoints
            lines = map (\(p, p') -> Line p p' thick undefined Nothing) linePoints
    toPrimitives pol@(Polygon {points=points, strokeLineThickness=thick, strokeColor=strokeColor, rotationM=Nothing})
        = toPrimitives (MultiLine allPoints thick strokeColor Nothing)
        where
            allPoints = allScreenPolygonPoints pol
    toPrimitives shape
        = map (rotateLeftAround rotatePoint angle) (toPrimitives shapePreRotate)
        where
            shapePreRotate = shape{rotationM=Nothing}
            (Just rotation@(Rotation _ angle)) = rotationM shape
            rotatePoint = findRotationPoint shapePreRotate rotation


class ToCenter a where
    toCenter :: a -> Point

instance ToCenter BoundingBox where
    toCenter bbox
        = Point (minX + 0.5 * (maxX - minX), minY + 0.5 * (maxY - minY))
        where
            minX = xMin bbox
            maxX = xMax bbox
            minY = yMin bbox
            maxY = yMax bbox

instance ToCenter Shape where
    toCenter c@(CompositeShape {positionM=(Just p), rotationM=Nothing})
        = p |+| center
        where
            center = toCenter c{positionM=Nothing}
    toCenter c@(CompositeShape {shapes=shapes, positionM=Nothing, rotationM=Nothing})
        = averagePoint centers
        where
            centers = map toCenter shapes
    toCenter r@(Rectangle {dimensions=(width, height), position=p, rotationM=Nothing})
        = p |+| (Point (0.5 * width, 0.5 * height))
    toCenter c@(Circle {position=p, rotationM=Nothing})
        = p
    toCenter po@(RegularPolygon {position=p, rotationM=Nothing})
        = p
    toCenter t@(Text {rotationM=Nothing})
        = (toCenter.toBoundingBox) t
    toCenter l@(Line {})
        = (toCenter.toBoundingBox) l
    toCenter ml@(MultiLine {})
        = (toCenter.toBoundingBox) ml
    toCenter a@(Polygon {})
        = (toCenter.toBoundingBox) a
    toCenter shape
        = rotateLeftAround rotationPoint angle center
        where
            (Just rotation) = rotationM shape
            shapePreRotate = shape{rotationM=Nothing}
            center = toCenter shapePreRotate
            rotationPoint = findRotationPoint shapePreRotate rotation
            (Rotation _ angle) = rotation


class (ToPrimitives a) => ToBoundingBox a where
    toBoundingBox :: a -> BoundingBox

instance ToBoundingBox BoundingBox where
    toBoundingBox box = box
    
instance ToBoundingBox Shape where
    toBoundingBox a
        = boundingBoxFromPrimitives $ toPrimitives a


class (ToBoundingBox a) => Overlaps a where
    {-
    The boundingbox of a1 partially overlaps the boundingbox of a2. Ofcourse if overlaps(a1, a2) then
    overlaps(a2, a1). However, if contains(a1, a2) or contains(a2, a1), then overlaps(a1, a2) == false
    -}
    overlaps :: (Overlaps b) => a -> b -> Bool
    overlaps a1 a2
        | contains a1 a2 || contains a2 a1 = False
        | xMax b1 < xMin b2 = False -- b1 is left of b2
        | xMin b1 > xMax b2 = False -- b2 is right of b2
        | yMax b1 < yMin b2 = False -- b1 is lower than b2
        | yMin b1 > yMax b2 = False -- b1 is higher than b2
        | otherwise = True
        where
            b1 = toBoundingBox a1
            b2 = toBoundingBox a2

    {-
    The boundingbox of a1 contains the boundingbox of a2. If boundingbox(a2) > boundingbox(a1)
    then a1 can never contain a2. If boundingbox(a2) == boundingbox(a1) and contains(a1, a2)
    then also contains(a2, a1).
    -}
    contains :: (Overlaps b) => a -> b -> Bool
    contains a1 a2
        | xMax b2 <= xMax b1 &&
          xMin b2 >= xMin b1 &&
          yMax b2 <= yMax b1 &&
          yMin b2 >= yMin b1 = True
        | otherwise = False
        where
            b1 = toBoundingBox a1
            b2 = toBoundingBox a2

    touches :: (Overlaps b) => a -> b -> Bool
    touches a1 a2 = overlaps a1 a2 || contains a1 a2 || contains a2 a1


instance Overlaps Shape
instance Overlaps BoundingBox


findRotationPoint :: (ToCenter a) => a -> Rotation -> Point
findRotationPoint a (Rotation AroundCenter _) = toCenter a
findRotationPoint _ (Rotation (AroundPoint p) _) = p

class ToCanvasOut a where
    toCanvasOut :: a -> CT.CanvasOut
    
instance ToCanvasOut BasicShapesOut where
    toCanvasOut (DrawShapes canvasId shapes) = CT.CanvasOperations canvasId (canvasOperations ++ [CT.Frame])
                where
                    canvasOperations = (concat.(map toCanvasOperations)) shapes
                    
  
class ToCanvasOperations a where
    toCanvasOperations :: a -> [CT.CanvasOperation]  


toCanvasText :: Shape -> CT.CanvasText
toCanvasText (Text {text=text, fontFamily=family_, fontSize=size, alignment=align})
    = CT.CanvasText text (CT.Font family_ $ round size) align


instance ToCanvasOperations Shape where
    toCanvasOperations (CompositeShape shapes Nothing Nothing)
        = (concat.(map toCanvasOperations)) shapes

    toCanvasOperations (CompositeShape shapes (Just translate) Nothing)
        = [ CT.DoTransform CT.Save
          , CT.DoTransform (CT.Translate screenPositionPoint)
          ] ++ drawOperations ++
          [ CT.DoTransform CT.Restore
          ]
        where
            screenPositionPoint = roundPoint translate
            drawOperations = toCanvasOperations (CompositeShape shapes Nothing Nothing)

    toCanvasOperations text@(Text { position=p
                                  , fillColor=fill
                                  , strokeLineThickness=thick
                                  , strokeColor=stroke
                                  , rotationM=Nothing
                                  })
        = [CT.DrawText canvasText p' textStroke textFill]
        where
          canvasText = toCanvasText text
          textFill = CT.TextFill (CT.CanvasColor screenFillColor)
          textStroke = CT.TextStroke (round thick) (CT.CanvasColor screenStrokeColor)
          screenStrokeColor = roundColor stroke
          screenFillColor = roundColor fill
          p' = roundPoint p

    toCanvasOperations shape
        -- Might be any rotated shape
        | isJust (rotationM shape) = [ CT.DoTransform CT.Save
                                     , CT.DoTransform (CT.Translate screenRotationPoint)
                                     , CT.DoTransform (CT.Rotate screenAngle)
                                     ]
                                   ++ (toCanvasOperations movedShape) ++
                                     [ CT.DoTransform CT.Restore
                                     ]
        -- Can only be Rectangle, Circle, RegularPolygon, Line, MultiLine or Polygon
        | isJust screenPathPartsM  = [CT.DrawPath startingPoint screenPathParts pathStroke canvasPathFill]
        | otherwise                = []
        where
            (Just rotation) = rotationM shape
            shapePreRotate = shape{rotationM = Nothing}
            rotationPoint = findRotationPoint shapePreRotate rotation
            screenRotationPoint = roundPoint rotationPoint
            (Rotation _ angle) = rotation
            screenAngle = round angle
            movedShape = translate (negateVector rotationPoint) shapePreRotate

            canvasPathFill = toCanvasPathFill shape
            screenPathPartsM = toScreenPathParts shape
            Just (screenPathParts, startingPoint) = screenPathPartsM
            screenStrokeColor = roundColor $ strokeColor shape
            thick = strokeLineThickness shape
            pathStroke = CT.PathStroke (round thick) (CT.CanvasColor screenStrokeColor)

          
class ToScreenPathPart a where
    toScreenPathParts :: a -> Maybe ([CT.ScreenPathPart], CT.ScreenStartingPoint)
    
instance ToScreenPathPart Shape where
    toScreenPathParts (Rectangle {position=p, dimensions=(w, h)})
        = Just ([CT.Rectangle p' (w', h')], p')
        where
            p' = roundPoint p
            w' = round w
            h' = round h
    toScreenPathParts (Circle {position=p, radius=r})
        = Just ([CT.Arc (p', r') 0 360], p')
        where
            p' = roundPoint p
            r' = round r
    toScreenPathParts (RegularPolygon {position=p, numberOfPoints=n, radius=r})
        = Just (lines ++ [CT.ClosePath], screenPoint)
        where
            polygonPoints = allRegularPolygonPoints n p r
            (screenPoint:ps) = map roundPoint polygonPoints
            lines = [CT.LineTo screenPoint' | screenPoint' <- (ps ++ [screenPoint])]
    toScreenPathParts (Line {point1=p1, point2=p2})
        = Just ([CT.LineTo p2'], p1')
        where
            p1' = roundPoint p1
            p2' = roundPoint p2
                                        
    toScreenPathParts (MultiLine {points=points})
        | (length points) > 0 = Just (lines ++ [CT.MoveTo p1'], p1')
        | otherwise           = Nothing
        where
            (p1':otherPoints') = map roundPoint points
            lines = [CT.LineTo p' | p' <- otherPoints']

    toScreenPathParts pol@(Polygon {points=points})
        | (length points) > 0 = Just (lines ++ [CT.MoveTo p1', CT.ClosePath], p1')
        | otherwise           = Nothing
        where
            allPoints = allScreenPolygonPoints pol
            (p1':otherPoints') = map roundPoint allPoints
            lines = [CT.LineTo p' | p' <- otherPoints']


toCanvasPathFill :: Shape -> CT.PathFill
toCanvasPathFill shape
    | hasCanvasPathFill shape = CT.PathFill (CT.CanvasColor screenFillColor)
    | otherwise               = CT.NoPathFill
    where
        fillColor_ = fillColor shape
        screenFillColor = roundColor fillColor_


hasCanvasPathFill :: Shape -> Bool
hasCanvasPathFill (Rectangle {})
    = True
hasCanvasPathFill (Circle {})
    = True
hasCanvasPathFill (RegularPolygon {})
    = True
hasCanvasPathFill (Polygon {})
    = True
hasCanvasPathFill _
    = False


allScreenPolygonPoints :: Shape -> [Point]
allScreenPolygonPoints (Polygon {points=points, strokeLineThickness=thick})
    | (length points) >= 2 = points ++ [closeP]
    | otherwise            = points
    where
        firstP = head points
        lastP  = last points
        closeP = followVector (0.5 * thick) (firstP |-| lastP) firstP



