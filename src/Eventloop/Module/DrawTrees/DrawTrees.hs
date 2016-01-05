module Eventloop.Module.DrawTrees.DrawTrees
    ( setupDrawTreesModuleConfiguration
    , drawTreesModuleIdentifier
    , drawTreesPostProcessor
    , showGeneralTreeList
    , rbExampleTree
    , roseExampleTree
    ) where

import Eventloop.Module.DrawTrees.Types
import Eventloop.Utility.Trees.LayoutTree
import Eventloop.Utility.Trees.GeneralTree
import Eventloop.Module.BasicShapes.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System


setupDrawTreesModuleConfiguration :: EventloopSetupModuleConfiguration
setupDrawTreesModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                         drawTreesModuleIdentifier
                                         Nothing
                                         Nothing
                                         Nothing
                                         (Just drawTreesPostProcessor)
                                         Nothing
                                         Nothing
                                      )

drawTreesModuleIdentifier :: EventloopModuleIdentifier
drawTreesModuleIdentifier = "parseTree"


drawTreesPostProcessor :: PostProcessor
drawTreesPostProcessor sharedConst sharedIOT ioConst ioStateT (OutDrawTrees (DrawTrees canvasId trees))
    = return [OutBasicShapes $ DrawShapes canvasId [shapeTrees]]
    where
        (shapeTrees, _, _) = showGeneralTreeList trees
        
drawTreesPostProcessor sharedConst sharedIOT ioConst ioStateT out
    = return [out]


maxWidth :: Int
maxWidth = 1024

showGeneralTreeList :: [GeneralTree] -> (Shape, RightOffset, BottomOffset)
showGeneralTreeList list = showGeneralTreeList' 0 0 0 0 list

showGeneralTreeList' :: LeftOffset -> TopOffset -> Float -> Int -> [GeneralTree] -> (Shape, RightOffset, BottomOffset)
showGeneralTreeList' _ _ _ _ [] = (CompositeShape [] Nothing Nothing, 0, 0)
showGeneralTreeList' left top maxBottom i (x:xs) | right <= 1024 || width > 1024 = (CompositeShape (treeShapes ++ gtrees) Nothing Nothing, rightOff', botOff')
                                                 | otherwise = showGeneralTreeList' 0 maxBottom' maxBottom' i (x:xs)
                                               where
                                                   maxBottom' = max maxBottom bottom
                                                   (CompositeShape treeShapes _ _, right, bottom) = showGeneralTree left top i x
                                                   (CompositeShape gtrees _ _, rightOff, botOff) = showGeneralTreeList' (right + marginBetweenTrees) top maxBottom' i' xs
                                                   i' = i + 1
                                                   width = right - left
                                                   rightOff' = max rightOff right
                                                   botOff'   = max bottom botOff

       
showGeneralTree :: LeftOffset -> TopOffset -> Int -> GeneralTree -> (Shape, RightOffset, BottomOffset)
showGeneralTree left top i tree = (CompositeShape [treeIndexShape, treeShape] Nothing Nothing, right, bottom)
                                where
                                    (ltree, right, bottom) = layoutGeneralTree left top tree
                                    treeShape = printTree ltree
                                    treeIndexShape = treeIndex i (left, top)

instance GeneralizeTree RBTree where
    generalizeTree (RBNode col _ []) = GeneralTreeBox content []
                                    where
                                        content = [GeneralNode (nodeColorToFillColor col) 5]
                                                
    generalizeTree (RBNode col str children) = GeneralTreeBox content children'WithLines
                                            where
                                                content = [GeneralNode (nodeColorToFillColor col) 20, GeneralNodeText (0,0,0, 255) str]
                                                children' = map generalizeTree children
                                                line = GeneralLine (0,0,0, 255)
                                                children'WithLines = zip (repeat line) children'

nodeColorToFillColor :: NodeColor -> FillColor
nodeColorToFillColor NodeRed   = (255, 0, 0, 255)
nodeColorToFillColor NodeBlack = (0, 0, 0, 255)
nodeColorToFillColor NodeGrey  = (125, 125, 125, 255)
                  
                 
instance GeneralizeTree RoseTree where
    generalizeTree (RoseNode str children) = GeneralTreeBox content children'WithLines
                                            where
                                                content = [GeneralNodeText (0,0,0, 255) str]
                                                children'WithLines = zip (repeat line) (map generalizeTree children)
                                                line = GeneralLine (0,0,0, 255)   
                        
rbExampleTree = RBNode NodeBlack "12"
                        [ RBNode NodeRed "10"
                                    [ RBNode NodeBlack "9"
                                                [ RBNode NodeBlack "" [],
                                                  RBNode NodeBlack "" []
                                                ],
                                      RBNode NodeBlack "11"
                                                [RBNode NodeBlack "" [],
                                                 RBNode NodeBlack "" []
                                                ]
                                    ],
                          RBNode NodeRed "13"
                                    [RBNode NodeBlack "36" [],
                                     RBNode NodeBlack "42"
                                               [RBNode NodeRed "36"
                                                          [RBNode NodeBlack "" [],
                                                           RBNode NodeBlack "" []
                                                          ],
                                                RBNode NodeBlack "" []
                                               ]
                                    ]
                        ]
                        
roseExampleTree = RoseNode "z"
                        [ RoseNode "aaa"
                                    [ RoseNode "bbb"
                                                [ RoseNode "ccc" [],
                                                  RoseNode "ddd" []
                                                ],
                                      RoseNode ""
                                                [RoseNode "fff" [],
                                                 RoseNode "ggg" [],
                                                 RoseNode "hhh" []
                                                ],
                                      RoseNode "iii"
                                                [RoseNode "" []
                                                ]
                                    ],
                          RoseNode "kkk"
                                    [RoseNode "lll" [],
                                     RoseNode "mmm"
                                               [RoseNode "nnn"
                                                          [RoseNode "q" [],
                                                           RoseNode "r" []
                                                          ],
                                                RoseNode "ooo" [],
                                                RoseNode "ppp" []
                                               ]
                                    ]
                        ]