module Eventloop.Module.Graphs.Graphs where


import Eventloop.Module.Graphs.Types
import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.Websocket.Mouse as M
import qualified Eventloop.Module.Websocket.Keyboard as K
import qualified Eventloop.Module.BasicShapes as BS
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Vectors


setupGraphsModuleConfiguration :: EventloopSetupModuleConfiguration
setupGraphsModuleConfiguration = ( EventloopSetupModuleConfiguration
                                      graphsModuleIdentifier
                                      Nothing
                                      Nothing
                                      (Just graphsPreProcessor)
                                      (Just graphsPostProcessor)
                                      Nothing
                                      Nothing
                                   )

graphsModuleIdentifier :: EventloopModuleIdentifier
graphsModuleIdentifier = "graphs"
                                      


nodeRadius   = 20 :: Float                 
textSize     = 16 :: Float
textFont     = "Courier"
xArrowSize   = 6 :: Float
yArrowSize   = 6 :: Float
weightHeight = 15 :: Float

dimCanvasGraphs = (840,440) :: (Float, Float)
roundDimCanvasGraphs = (round $ fst dimCanvasGraphs, round $ snd dimCanvasGraphs) :: (Int, Int)
canvasGraphsWidth = fst dimCanvasGraphs
canvasGraphsHeight = snd dimCanvasGraphs

instructionsHeight = 200 :: Float
instructionsBeginAt = instructionsHeight + canvasGraphsHeight
canvasInstrWidth = canvasGraphsWidth
canvasInstrHeight = instructionsHeight * 2 + canvasGraphsHeight
dimCanvasInstr = (canvasInstrWidth, canvasInstrHeight)
roundDimCanvasInstr = (round $ fst dimCanvasInstr, round $ snd dimCanvasInstr) :: (Int, Int)

canvasIdGraphs = 1 :: C.CanvasId
canvasIdInstructions = 2 :: C.CanvasId

               
-- | Checkes to see if there is a node on a certain position                
onNode :: [Node] -> Pos -> Maybe Node
onNode [] _ = Nothing
onNode (n@(_, (nx, ny), _):ns) (x,y) | difference <= nodeRadius = Just n
                                     | otherwise                = onNode ns (x,y)
                                    where
                                        dx         = nx - x
                                        dy         = ny - y
                                        difference = sqrt (dx^2 + dy^2)
                 


-- | Abstracts the standardized 'EventLoop.Types.EventTypes' to 'GraphsIn'
graphsPreProcessor :: PreProcessor
graphsPreProcessor sharedConst sharedIOT ioConst ioStateT (InMouse (M.Mouse M.MouseCanvas 1 event (Point p)))
    | x >=0 && y >= 0 && y <= canvasGraphsHeight && x <= canvasGraphsWidth = return [InGraphs $ Mouse event p]
    | otherwise = return []
    where
        (x, y) = p
    
graphsPreProcessor sharedConst sharedIOT ioConst ioStateT k@(InKeyboard (K.Key key))
    = return [k, InGraphs $ Key key]
    
graphsPreProcessor sharedConst sharedIOT ioConst ioStateT inEvent
    = return [inEvent]


-- | Abstracts 'GraphsOut' back to 'BasicShapes' and 'Canvas' events
graphsPostProcessor :: PostProcessor
graphsPostProcessor sharedConst sharedIOT ioConst ioStateT (OutGraphs SetupGraphs)
    = return [ OutCanvas $ C.SetupCanvas canvasIdGraphs  1 roundDimCanvasGraphs (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
             , OutCanvas $ C.SetupCanvas canvasIdInstructions 2 roundDimCanvasInstr (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
             ]
        
                   
graphsPostProcessor sharedConst sharedIOT ioConst ioStateT (OutGraphs (DrawGraph graph))
    = return [ OutCanvas $ C.CanvasOperations canvasIdGraphs [C.Clear C.ClearCanvas]
             , OutBasicShapes $ BS.DrawShapes canvasIdGraphs shapes
             ]
    where
        shapes = graphToShapes graph
    
graphsPostProcessor sharedConst sharedIOT ioConst ioStateT (OutGraphs (Instructions is))
    = return [ OutCanvas $ C.CanvasOperations canvasIdInstructions [C.Clear C.ClearCanvas]
             , OutBasicShapes $ BS.DrawShapes canvasIdInstructions shapes
             ]
    where
        startPLine  = Point (0, 0)
        endPLine    = Point (canvasGraphsWidth, 0)
        lineHeight  = 2
        lineShape   = BS.BaseShape (BS.Line startPLine endPLine) lineHeight (0,0,0,255) Nothing
        textShape   = (\line p -> BS.BaseShape (textPrim line p) 0 (0,0,0,0) Nothing) 
        textPrim    = (\line p -> BS.Text line textFont textSize p (0,0,0,255))
        textMargin        = 2
        heights           = iterate ((+) (textSize + textMargin)) lineHeight
        isAndHeights      = zip is heights
        instructionShapes = map (\(line, top) -> textShape line $ Point (0.5 * canvasGraphsWidth, top)) isAndHeights
        shapes            = [BS.CompositeShape (lineShape:instructionShapes) (Just (Point (0, instructionsBeginAt))) Nothing]

graphsPostProcessor sharedConst sharedIOT ioConst ioStateT out
    = return [out]
  
-- | Translates color datatype to RGBA codes
colorToRGBAColor :: Color -> BS.Color
colorToRGBAColor Red    = (255, 0, 0, 255)
colorToRGBAColor Blue   = (0, 0, 255, 255)
colorToRGBAColor Green  = (0, 255, 0, 255)
colorToRGBAColor Purple = (255, 0, 255, 255)
colorToRGBAColor Grey   = (125, 125, 125, 255)
colorToRGBAColor Yellow = (255, 255, 0, 255)
colorToRGBAColor Orange = (255, 125, 0, 255)
colorToRGBAColor Black  = (0, 0, 0, 255)
colorToRGBAColor White  = (255, 255, 255, 255)
     
     
-- | Translates the thickness to a float           
thicknessToFloat :: Thickness -> BS.StrokeLineThickness
thicknessToFloat Thick = 3.0
thicknessToFloat Thin  = 1.0


findNode :: [Node] -> Label -> Node
findNode [] l = error ("Tried to find a node in the graph with label '" ++ (show l) ++ "' but could not find it!")
findNode (n@(ln, _, _):ns) l | l == ln = n
                             | otherwise = findNode ns l
                             

graphToShapes :: Graph -> [BS.Shape]
graphToShapes graph
    = (concat nodeShapes) ++ (concat edgeShapes)
    where
        allNodes = nodes graph
        allEdges = edges graph
        isDirected = directed graph
        isWeighted = weighted graph
        allEdgesWithNodes = map (\e@(l1, l2,_,_,_) -> (findNode allNodes l1, findNode allNodes l2, e)) allEdges
        nodeShapes = map nodeToShapes allNodes
        edgeShapes = map (\(n1, n2, e) -> edgeToShapes n1 n2 e isDirected isWeighted) allEdgesWithNodes


nodeToShapes :: Node -> [BS.Shape]
nodeToShapes (l, p, col)
    = [BS.BaseShape nodePrim 2 (0,0,0,255) Nothing, BS.BaseShape textPrim 3 (0,0,0,255) Nothing]
    where
        color = colorToRGBAColor col
        lStr = [l]
        nodePrim = BS.Circle (Point p) nodeRadius color
        textPrim = BS.Text lStr textFont textSize (Point p) (0,0,0,255)

        
edgeToShapes :: Node -> Node -> Edge -> Directed -> Weighted -> [BS.Shape]
edgeToShapes (_, p1, _) (_, p2, _) (_, _, col, w, thick) directed weighted
    = lineShape:(weightShapes ++ directShapes)
    where
        directShapes | directed == Directed   = [arrowShape arrow1Prim, arrowShape arrow2Prim]
                     | directed == Undirected = []
                    where
                        arrow1Prim = BS.Line (Point arrowStart) (Point arrow1End)
                        arrow2Prim = BS.Line (Point arrowStart) (Point arrow2End)
                        arrowShape prim = BS.BaseShape prim thickness color Nothing
        weightShapes | weighted == Weighted   = [BS.BaseShape weightPrim 0 (0,0,0,0) Nothing]
                     | weighted == Unweighted = []
                    where
                        wStr = show w
                        weightPrim = BS.Text wStr textFont textSize (Point textPos) (0,0,0,255)
        lineShape = BS.BaseShape linePrim thickness color Nothing
                where
                    linePrim = BS.Line (Point lineStart) (Point lineEnd)
        thickness = thicknessToFloat thick
        color = colorToRGBAColor col
        -- Margin line vector stuff
        lineVector         = vectorize p1 p2
        lineVector'        = vectorize p2 p1
        lineStart          = posOnVector nodeRadius lineVector p1
        lineEnd            = posOnVector nodeRadius lineVector' p2
        -- Arrow directed vector stuff
        arrowPerpStart     = posOnVector xArrowSize lineVector' lineEnd  
        upPerpLineVector   = upPerpendicularTo p1 p2
        downPerpLineVector = downPerpendicularTo p1 p2
        arrowStart         = lineEnd
        arrow1End          = posOnVector yArrowSize upPerpLineVector arrowPerpStart  
        arrow2End          = posOnVector yArrowSize downPerpLineVector arrowPerpStart
        -- Weight vector stuff
        halfSize          = vectorSize lineVector' / 2
        textPerpStart     = posOnVector halfSize lineVector p1
        textPos           = posOnVector weightHeight upPerpLineVector textPerpStart
        

-- | Returns the point when making a step f long from the point start in the direction of the vector. The length between start pos and result pos is always f.
posOnVector :: Float -> Vector -> Pos -> Pos
posOnVector f (xv, yv) (xStart, yStart) = (x, y)
                                        where
                                            x        = xStart + fraction * xv
                                            y        = yStart + fraction * yv
                                            fraction = f / size
                                            size     = vectorSize (xv, yv)

-- | Vector from p1 to p2 
vectorize :: Pos -> Pos -> Vector
vectorize (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)


-- | Returns the vector perpendicular on the given vector between the 2 points. Always has positive y and vector length 1; y is inverted in canvas
downPerpendicularTo :: Pos -> Pos -> Vector
downPerpendicularTo (x1, y1) (x2, y2) | y2 > y1   = ((-1) * sign * (abs yv) / size, (abs xv) / size)
                                    | otherwise = (       sign * (abs yv) / size, (abs xv) / size)
                                      where
                                          (xv, yv) = vectorize (x1, y1) (x2, y2)
                                          size     = vectorSize (xv, yv)
                                          sign     = case xv of
                                                        0 -> (-1)
                                                        _ -> xv / (abs xv)
                                            
                                            
-- | Returns the vector perpendicular on the given vector between the 2 points. Always has negative y and vector length 1; y is inverted in canvas
upPerpendicularTo :: Pos -> Pos -> Vector
upPerpendicularTo p1 p2 = ((-1) * xp, (-1) * yp)
                        where
                            (xp, yp) = downPerpendicularTo p1 p2
                          
-- | Returns the size of the vector                          
vectorSize :: Vector -> Float
vectorSize (x, y) = sqrt (x^2 + y^2)                                            