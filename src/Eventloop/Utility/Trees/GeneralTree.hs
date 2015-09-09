module Eventloop.Utility.Trees.GeneralTree where

import Eventloop.Module.BasicShapes.Types
import Eventloop.Utility.Vectors
import Eventloop.Utility.Trees.LayoutTree


data GeneralTree = GeneralTreeBox [GeneralNodeContent] [(GeneralLine, GeneralTree)] 
                deriving (Show, Eq)

data GeneralNodeContent = GeneralNodeText FillColor String
                        | GeneralNode FillColor Radius
                        deriving (Show, Eq)
                        
data GeneralLine = GeneralLine StrokeColor
                    deriving (Show, Eq)
                    
                    
type LeftOffset   = X
type TopOffset    = Y
type RightOffset  = X
type BottomOffset = Y
type Middle = GraphicalNumeric

type Pos = (X, Y)

class GeneralizeTree a where
    generalizeTree :: a -> GeneralTree
    
instance GeneralizeTree GeneralTree where
    generalizeTree a = a
    
    
    
-- Courier 16px
textFont = "Courier"
textHeight = 16 :: Float -- px
charWidth  = 10 :: Float

marginBetweenTrees = 10 :: Float
marginBetweenNodeContent = 2 :: Float
marginBetweenNodeRows = 20 :: Float
marginBetweenNodeColumns = 20 :: Float




generalNodeDimension :: GeneralTree -> Dimensions
generalNodeDimension (GeneralTreeBox content _) = flattenDimensions contentDimensions
                                                where
                                                    contentDimensions = map generalNodeContentDimension content

                                                    
flattenDimensions :: [Dimensions] -> Dimensions
flattenDimensions [] = (0.0,0.0)
flattenDimensions [d] = d
flattenDimensions ((w,h):ds) = (max w wTotal, h + marginBetweenNodeContent + hTotal)
                            where
                                (wTotal, hTotal) = flattenDimensions ds
                             
                             
generalNodeContentDimension :: GeneralNodeContent -> Dimensions
generalNodeContentDimension (GeneralNodeText _ str) = textSize str
generalNodeContentDimension (GeneralNode _ r      ) = (2*r, 2*r)
                       
                       
layoutGeneralTree :: LeftOffset -> TopOffset -> GeneralTree -> (LayoutTree, RightOffset, BottomOffset)
layoutGeneralTree leftOffset topOffset box@(GeneralTreeBox content children) = (LBox (Point (x, y)) topConnect bottomConnect lcs lchildrenWithLines, rightOffset, bottomOffsetChildren)
                                                                        where
                                                                            x               = leftBorder
                                                                            y               = topOffset
                                                                            middle          = (rightOffset - leftOffset) / 2 + leftOffset
                                                                            rightOffset     = max rightOffsetChildren (leftOffset + width)
                                                                            leftBorder      = middle - (width / 2)
                                                                            topConnect      = Point (width / 2, 0)
                                                                            bottomConnect   = Point (width / 2, height)
                                                                            (width, height) = generalNodeDimension box
                                                                            lcs             = layoutGeneralNodeContentList (width / 2) 0 content
                                                                            (lchildrenWithLines, rightOffsetChildren, bottomOffsetChildren) = layoutGeneralTreeChildren leftOffset (topOffset + marginBetweenNodeRows + height) children

layoutGeneralTreeChildren :: LeftOffset -> TopOffset -> [(GeneralLine, GeneralTree)] -> ([(LayoutLine, LayoutTree)], RightOffset, BottomOffset)
layoutGeneralTreeChildren left top treesWithLines =(zip lLines lTrees, right, bottom)
                                    where
                                        lines        = map fst treesWithLines
                                        generalTrees = map snd treesWithLines
                                        lLines       = map layoutLine lines
                                        (lTrees, right, bottom) = layoutGeneralTrees left top generalTrees
                                        
layoutLine :: GeneralLine -> LayoutLine
layoutLine (GeneralLine color) = LayoutLine color 
                                                                            
layoutGeneralTrees :: LeftOffset -> TopOffset -> [GeneralTree] -> ([LayoutTree], RightOffset, BottomOffset)
layoutGeneralTrees left top [] = ([], left, top)
layoutGeneralTrees left top [box] = (\(a,b,c) -> ([a],b,c)) $ layoutGeneralTree left top box
layoutGeneralTrees left top (box:bs) = (lbox:lrest, rightRest, max bottom bottomRest)
                                            where
                                                (lbox, right , bottom) = layoutGeneralTree left top box
                                                (lrest, rightRest, bottomRest) = (layoutGeneralTrees (right + marginBetweenNodeColumns) top bs)
                                      
layoutGeneralNodeContentList :: Middle -> Height -> [GeneralNodeContent] -> [LayoutNodeContent]
layoutGeneralNodeContentList _ _ [] = []
layoutGeneralNodeContentList middle height [nc] = [layoutGeneralNodeContent (middle, height) nc]
layoutGeneralNodeContentList middle height (nc:ncs) = lnc:(layoutGeneralNodeContentList middle height' ncs)
                                                    where
                                                        height' = height + ncHeight + marginBetweenNodeContent
                                                        (_, ncHeight) = generalNodeContentDimension nc
                                                        lnc = layoutGeneralNodeContent (middle, height) nc
                                                                            
layoutGeneralNodeContent :: Pos -> GeneralNodeContent -> LayoutNodeContent
layoutGeneralNodeContent (middle, top) gnc@(GeneralNodeText color str) = LayoutNodeText color (Point (x,y)) str (textSize str)
                                                                    where
                                                                        x = middle
                                                                        y = top
                                                                        (w, h) = generalNodeContentDimension gnc
                                                                        
layoutGeneralNodeContent (middle, top) gnc@(GeneralNode color rad)     = LayoutNode color (Point (x,y)) rad
                                                                where
                                                                    x = middle
                                                                    y = top + h/2
                                                                    (w, h) = generalNodeContentDimension gnc

-- (width, height)                        
textSize :: [Char] -> (Float, Float)
textSize str = (textWidth, textHeight)
             where
                textWidth = charWidth * (fromIntegral (length str))


treeIndex :: Int -> Offset -> Shape
treeIndex i (x, y) = BaseShape (Text iStr "Courier" 20 p (255,75,75, 255)) 1 (0,0,0,0) Nothing
                where
                    iStr = show i
                    (wText, hText) = textSize iStr
                    p = Point (x + 0.5 * wText, y)
