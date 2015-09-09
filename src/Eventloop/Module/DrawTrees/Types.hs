module Eventloop.Module.DrawTrees.Types where

import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Utility.Trees.GeneralTree


data DrawTreesOut = DrawTrees CanvasId [GeneralTree]
                  deriving (Show, Eq)
             
          
data NodeColor = NodeRed
               | NodeBlack
               | NodeGrey
               deriving (Show, Eq)

data RBTree = RBNode NodeColor String [RBTree]
            deriving (Show, Eq)
            
            
data RoseTree = RoseNode String [RoseTree]
                deriving (Show, Eq)



                

