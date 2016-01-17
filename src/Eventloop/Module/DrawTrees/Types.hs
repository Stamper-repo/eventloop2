{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.DrawTrees.Types where

import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Utility.Trees.GeneralTree

import GHC.Generics (Generic)
import Control.DeepSeq


data DrawTreesOut = DrawTrees CanvasId [GeneralTree]
                  deriving (Show, Eq, Generic, NFData)
             
          
data NodeColor = NodeRed
               | NodeBlack
               | NodeGrey
               deriving (Show, Eq, Generic, NFData)

data RBTree = RBNode NodeColor String [RBTree]
            deriving (Show, Eq)
            
            
data RoseTree = RoseNode String [RoseTree]
                deriving (Show, Eq)



                

