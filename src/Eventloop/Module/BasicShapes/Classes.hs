module Eventloop.Module.BasicShapes.Classes where

import Control.Concurrent.MVar

import Eventloop.Utility.Vectors
import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.BasicShapes.MeasureTextHack
import qualified Eventloop.Module.Websocket.Canvas.Types as CT


addBoundingBox :: BoundingBox -> BoundingBox -> BoundingBox
addBoundingBox (BoundingBox p11 p21 p31 p41) (BoundingBox p12 p22 p32 p42) = BoundingBox (Point (xMin, yMin)) (Point (xMin, yMax)) (Point (xMax, yMax)) (Point (xMax, yMin))
                                                        where
                                                            allPoints = [p11, p21, p31, p41, p12, p22, p32, p42]
                                                            xs = map (\(Point (x,y)) -> x) allPoints
                                                            ys = map (\(Point (x,y)) -> y) allPoints
                                                            xMin = minimum xs
                                                            xMax = maximum xs
                                                            yMin = minimum ys
                                                            yMax = maximum ys

foldBoundingBoxes :: (BoundingBox -> BoundingBox -> BoundingBox) -> [BoundingBox] -> BoundingBox
foldBoundingBoxes _ [] = error "Tried to fold zero bounding boxes which is no bounding box. Undefined!"
foldBoundingBoxes op (box:bs) = foldl op box bs
              

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



allPolygonPoints :: AmountOfPoints -> Point -> Radius -> [Point]
allPolygonPoints n centralPoint r | n < 1 = error "A polygon with 0 or more sides doesn't exist!"
                                  | otherwise = [centralPoint |+| (toPoint (PolarCoord (r, angle)))  |angle <- anglesRads]
                                where
                                    anglePart = 360 / (fromIntegral n)
                                    startAngle = 0
                                    anglesDeg = filter (< 360) [startAngle, startAngle + anglePart..360]
                                    anglesRads = map degreesToRadians anglesDeg


boundingBoxFromPoints :: [Point] -> BoundingBox
boundingBoxFromPoints points
    = BoundingBox (Point (xMin, yMin)) (Point (xMin, yMax)) (Point (xMax, yMax)) (Point (xMax, yMin))
    where
        xs = map x points
        ys = map y points
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys


roundPoint :: Point -> CT.ScreenPoint
roundPoint (Point (x, y)) = (round x, round y)


roundColor :: Color -> CT.ScreenColor
roundColor (r, b, g, a) = (round r, round b, round g, a)

instance Translate BoundingBox where
    translate pTrans = opOnBoundingBox ((|+|) pTrans)

instance Translate Shape where
    translate p b@(BaseShape {primitive=prim})          = b {primitive = translate p prim}
    translate p (CompositeShape shapes Nothing rotM)    = CompositeShape shapes (Just p) rotM
    translate p2 (CompositeShape shapes (Just p1) rotM) = CompositeShape shapes (Just $ p1 |+| p2) rotM
    
instance Translate Primitive where
    translate p r@(Rectangle {translation=trans}) = r {translation = trans |+| p}
    translate p c@(Circle {translation=trans})    = c {translation = trans |+| p}
    translate p po@(Polygon {translation=trans})  = po {translation = trans |+| p}
    translate p t@(Text {translation=trans})      = t {translation = trans |+| p}
    translate pTrans (Line p1 p2)                 = Line (p1 |+| pTrans) (p2 |+| pTrans)
    translate pTrans (MultiLine p1 p2 ops)        = MultiLine (p1 |+| pTrans) (p2 |+| pTrans) (map ((|+|) pTrans) ops)
      
      
instance RotateLeftAround BoundingBox where               
    rotateLeftAround rotatePoint aDeg box = opOnBoundingBox (rotateLeftAround rotatePoint aDeg) box
    

class (ToBoundingBox a) => ToCenter a where
    toCenter :: a -> Point

instance ToCenter Primitive where
    toCenter = toCenter.toBoundingBox
    
instance ToCenter Shape where
    toCenter = toCenter.toBoundingBox

instance ToCenter BoundingBox where
    toCenter (BoundingBox (Point (x1, y1)) (Point (x2, y2)) _ (Point (x4, y4))) = Point (x1 + 0.5 * w, y1 + 0.5 * h)
                                                            where
                                                                w = x4 - x1
                                                                h = y2 - y1
    
    
class ToBoundingBox a where
    toBoundingBox :: a -> BoundingBox

instance ToBoundingBox BoundingBox where
    toBoundingBox box = box
    
instance ToBoundingBox Primitive where
    toBoundingBox (Rectangle (Point (x, y)) (w, h) _) = BoundingBox (Point (x, y)) (Point (x, y + h)) (Point (x + w, y + h)) (Point (x + w, y))
    toBoundingBox (Circle p r f) = toBoundingBox (Rectangle (p |-| (Point (r, r))) (2 * r, 2 * r) f)
    toBoundingBox (Polygon a p r _)
         = boundingBoxFromPoints polyPoints
         where
            polyPoints = allPolygonPoints a p r


    toBoundingBox text@(Text _ _ _ p _)
        = BoundingBox p (Point (x, y + height)) (Point (x + width, y + height)) (Point (x + width, y))
        where
            canvasText = toCanvasText text
            (width_, height_) = useMeasureText canvasText
            width = fromIntegral width_
            height = fromIntegral height_
            Point (x, y) = p

    toBoundingBox (Line p1 p2) = toBoundingBox (MultiLine p1 p2 []) 
    toBoundingBox (MultiLine p1 p2 ops)
        = boundingBoxFromPoints (p1:p2:ops)
                                        
instance ToBoundingBox Shape where
    toBoundingBox (BaseShape prim _ _ (Just rotation)) = rotatedBox
                                                       where
                                                           baseBox = toBoundingBox prim
                                                           rotationPoint = findRotationPoint prim rotation
                                                           (Rotation _ angle) = rotation
                                                           rotatedBox = rotateLeftAround rotationPoint angle baseBox
    toBoundingBox (BaseShape prim _ _ Nothing) = toBoundingBox prim
    toBoundingBox (CompositeShape shapes (Just translation) (Just rotation)) = rotateLeftAround rotationPoint angle translatedBox
                                                                            where
                                                                               baseBox = toBoundingBox (CompositeShape shapes Nothing Nothing)
                                                                               translatedBox = translate translation baseBox
                                                                               rotationPoint = findRotationPoint translatedBox rotation
                                                                               (Rotation _ angle) = rotation
    toBoundingBox (CompositeShape shapes (Just translation) Nothing) = translate translation baseBox
                                                                    where
                                                                        baseBox = toBoundingBox (CompositeShape shapes Nothing Nothing)
    toBoundingBox (CompositeShape shapes Nothing (Just rotation)) = rotateLeftAround rotationPoint angle baseBox
                                                                where
                                                                    baseBox = toBoundingBox (CompositeShape shapes Nothing Nothing)
                                                                    rotationPoint = findRotationPoint baseBox rotation
                                                                    (Rotation _ angle) = rotation
    toBoundingBox (CompositeShape shapes Nothing Nothing) = foldBoundingBoxes addBoundingBox $ map toBoundingBox shapes



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
instance Overlaps Primitive
instance Overlaps BoundingBox

{-
Er is een probleem met waar de rotationpoint staat.
De boundingbox van CompositeShapes ziet er vreemd uit.
Het probleem kan zijn omdat er ergens nog de oude manier van boundingboxes staat, maar waar?
Als de boundingbox weer correct is, wordt het rotationpoint ook weer correct.

Daarnaast moet er nog even nagedacht worden over de translatie en rotatiepunt van op elkaar gestapelde
composite shapes. Wordt de boundingbox lokaal uitgerekend? Wordt het rotatiepunt lokaal uitgerekent?
Meerdere translaties op elkaar in canvas, kan dat of is de translatie steeds vanuit het oude origin?

Fix 1: De boundingboxes van shapes in compositeshape samen voegen zonder de translatie van die compositeshape.
Daarmee krijg je de locale boundingbox met de correcte center punt.

Waarom ik een boundingbox met de verkeerde punten op de verkeerde plekken krijg, blijft een raadsel.
Even kijken of foldBoundingBoxes goed werkt of niet

-}                                                                                   

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


toCanvasText :: Primitive -> CT.CanvasText
toCanvasText (Text text fontF fontS _ _)
    = CT.CanvasText text (CT.Font fontF $ round fontS) CT.AlignLeft


instance ToCanvasOperations Shape where
    toCanvasOperations (BaseShape prim lineThick color (Just rotation)) 
        | angle == 0 = toCanvasOperations (BaseShape prim lineThick color Nothing)
        | otherwise = [ CT.DoTransform CT.Save
                      , CT.DoTransform (CT.Translate screenRotationPoint)
                      , CT.DoTransform (CT.Rotate screenAngle)
                      ] ++ movedDrawOperations ++
                      [ CT.DoTransform CT.Restore
                      ]
        where
            rotationPoint = findRotationPoint prim rotation
            screenRotationPoint = roundPoint rotationPoint
            movedPrim = translate (negateVector rotationPoint) prim 
            movedDrawOperations = toCanvasOperations (BaseShape movedPrim lineThick color Nothing)
            (Rotation _ angle) = rotation
            screenAngle = round angle
            
    -- Rotation is broken. When we translated to rotationpoint, translation of prim should be adjusted accordingly (prim' = move prim (-rotationPoint))
                                                                
    toCanvasOperations (BaseShape text@(Text _ _ _ p fillColor) lineThick strokeColor Nothing)
        = [CT.DrawText canvasText p' textStroke textFill]
        where
          canvasText = toCanvasText text
          textFill = CT.TextFill (CT.CanvasColor screenFillColor)
          textStroke = CT.TextStroke (round lineThick) (CT.CanvasColor screenStrokeColor)
          screenStrokeColor = roundColor strokeColor
          screenFillColor = roundColor fillColor
          p' = roundPoint p

    toCanvasOperations (BaseShape prim lineThick strokeColor Nothing) = case fillColorM of
                                                                            (Just fillColor') -> [CT.DrawPath startingPoint screenPathParts pathStroke pathFill]
                                                                                              where
                                                                                                  screenFillColor = roundColor fillColor'
                                                                                                  pathFill = CT.PathFill (CT.CanvasColor screenFillColor)
                                                                            Nothing           -> [CT.DrawPath startingPoint screenPathParts pathStroke CT.NoPathFill]
                                                                      where
                                                                          (screenPathParts, startingPoint, fillColorM) = toScreenPathParts prim
                                                                          pathStroke = CT.PathStroke (round lineThick) (CT.CanvasColor screenStrokeColor)
                                                                          screenStrokeColor = roundColor strokeColor

    toCanvasOperations (CompositeShape shapes Nothing Nothing) = (concat.(map toCanvasOperations)) shapes

    toCanvasOperations c@(CompositeShape shapes (Just translation) (Just rotation))
        | angle == 0 = toCanvasOperations (CompositeShape shapes (Just translation) Nothing)
        | otherwise  = [ CT.DoTransform CT.Save
                       , CT.DoTransform (CT.Translate screenTotalTranslation)
                       , CT.DoTransform (CT.Rotate screenAngle)
                       ] ++ movedDrawOperations ++
                       [ CT.DoTransform CT.Restore
                       ]
        where
            rotationPoint = findRotationPoint c rotation
            movedShapes = map (translate (negateVector rotationPoint)) shapes
            movedDrawOperations = toCanvasOperations (CompositeShape movedShapes Nothing Nothing)
            (Rotation _ angle) = rotation
            screenAngle = round angle
            screenTotalTranslation = roundPoint (translation |+| rotationPoint)
            
    toCanvasOperations c@(CompositeShape shapes Nothing (Just rotation))
        | angle == 0 = toCanvasOperations (CompositeShape shapes Nothing Nothing)
        | otherwise  = [ CT.DoTransform CT.Save
                       , CT.DoTransform (CT.Translate screenRotationPoint)
                       , CT.DoTransform (CT.Rotate screenAngle)
                       ] ++ movedDrawOperations ++
                       [ CT.DoTransform CT.Restore
                       ]
        where
            rotationPoint = findRotationPoint c rotation
            movedShapes = map (translate (negateVector rotationPoint)) shapes
            movedDrawOperations = toCanvasOperations (CompositeShape movedShapes Nothing Nothing)
            (Rotation _ angle) = rotation
            screenAngle = round angle
            screenRotationPoint = roundPoint rotationPoint
            
    toCanvasOperations (CompositeShape shapes (Just translate) Nothing)
        = [ CT.DoTransform CT.Save
          , CT.DoTransform (CT.Translate screenTranslationPoint)
          ] ++ drawOperations ++
          [ CT.DoTransform CT.Restore
          ]
        where
            screenTranslationPoint = roundPoint translate
            drawOperations = toCanvasOperations (CompositeShape shapes Nothing Nothing)
          
          
class ToScreenPathPart a where
    toScreenPathParts :: a -> ([CT.ScreenPathPart], CT.ScreenStartingPoint, Maybe FillColor)    
    
instance ToScreenPathPart Primitive where
    toScreenPathParts (Rectangle p (w, h) f) = ([CT.Rectangle p' (w', h')], p', Just f)
                                             where
                                                 p' = roundPoint p
                                                 w' = round w
                                                 h' = round h
    toScreenPathParts (Circle p r f) = ([CT.Arc (p', r') 0 360], p', Just f)
                                     where
                                         p' = roundPoint p
                                         r' = round r
    toScreenPathParts (Polygon n p r f) = (lines, screenPoint, Just f)
                                        where
                                            polygonPoints = allPolygonPoints n p r
                                            (screenPoint:ps) = map roundPoint polygonPoints
                                            lines = [CT.LineTo screenPoint' | screenPoint' <- (ps ++ [screenPoint])]
    toScreenPathParts (Text {}) = error "Text is stupid and not implemented the same way in JS canvas"
    toScreenPathParts (Line p1 p2) = ([CT.LineTo p2'], p1', Nothing)
                                    where
                                        p1' = roundPoint p1
                                        p2' = roundPoint p2
                                        
    toScreenPathParts (MultiLine p1 p2 otherPoints) = (lines ++ [CT.MoveTo p1'], p1', Nothing)
                                                    where
                                                        allPoints = p1:p2:otherPoints
                                                        (p1':otherPoints') = map roundPoint allPoints
                                                        lines = [CT.LineTo p' | p' <- otherPoints']
    
    
    