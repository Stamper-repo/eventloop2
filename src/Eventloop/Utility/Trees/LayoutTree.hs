module Eventloop.Utility.Trees.LayoutTree where

import Eventloop.Module.BasicShapes.Types
import Eventloop.Utility.Vectors

data LayoutTree = LBox Point TopConnect BottomConnect [LayoutNodeContent] [(LayoutLine, LayoutTree)] 
                deriving (Show, Eq)

data LayoutNodeContent = LayoutNodeText FillColor Point String Dimensions
                       | LayoutNode FillColor Point Radius
                       deriving (Show, Eq)
                       
data LayoutLine = LayoutLine StrokeColor
                deriving (Show, Eq)
                
type TopConnect = Connect
type BottomConnect = Connect
type Connect = Point


marginLine = 3 :: Float
lineThickness = 1 :: Float
textThickness = 1 :: Float
textFont = "Courier"


printTree :: LayoutTree -> Shape
printTree (LBox (Point offset) _ botConnect nodeContents childrenWithLines) = CompositeShape (shapeChildren ++ shapeLines ++ shapeContents) Nothing Nothing
                                                where
                                                    children = map snd childrenWithLines
                                                    shapeChildren = map printTree children
                                                    shapeContents = map (printNodeContent offset) nodeContents
                                                    shapeLines = map (printLine (botConnect')) childrenWithLines
                                                    botConnect' = (Point offset) |+| botConnect
                                                    

                                                    
printNodeContent :: Offset -> LayoutNodeContent -> Shape
printNodeContent (xOffset, yOffset) (LayoutNodeText fillColor p text (_, height)) = Text text textFont height ((Point (xOffset, yOffset)) |+| p) AlignCenter fillColor textThickness (0,0,0,0) Nothing
printNodeContent (xOffset, yOffset) (LayoutNode fillColor p r)                    = Circle ((Point (xOffset, yOffset)) |+| p) r fillColor lineThickness (0,0,0,0) Nothing


printLine :: Point -> (LayoutLine, LayoutTree) -> Shape
printLine startPoint ((LayoutLine lineColor),(LBox point topConnect _ _ _)) = Line startMarg endMarg lineThickness lineColor Nothing
                                                                        where
                                                                            startMarg = marginizeLinePoints marginLine startPoint endPoint
                                                                            endMarg   = marginizeLinePoints marginLine endPoint startPoint
                                                                            endPoint  = point |+| topConnect

                                                                            
marginizeLinePoints :: GraphicalNumeric -> Point -> Point -> Point
marginizeLinePoints margin p1@(Point (xStart, yStart)) p2@(Point (xEnd, yEnd)) = Point (xStart', yStart')
                            where
                                xStart'  = xStart + fraction * xSize
                                yStart'  = yStart + fraction * ySize
                                fraction = margin / size
                                size     = lengthBetweenPoints p1 p2
                                (xSize, ySize) = differenceBetweenPoints p1 p2