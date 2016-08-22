{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.Graphs.Types
    ( module Eventloop.Module.Graphs.Types
    , M.MouseEvent(..)
    , M.MouseButton(..)
    ) where

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Eventloop.Module.Websocket.Mouse.Types as M
    
type Pos = (Float, Float)
type Vector = (Float, Float)


data GraphsIn = Mouse M.MouseEvent Pos
              | Key [Char]
              deriving (Eq, Show)
    
data GraphsOut = SetupGraphs
               | DrawGraph Graph
               | Instructions [String]
               deriving (Eq, Show, Generic, NFData)
              
----- Graph -----
type Label   = Char
type Weight  = Float

type Node = (Label, Pos, Color)
type Edge = (Label, Label, Color, Weight, Thickness)

data Graph = Graph
            { nodes    :: [Node]
            , edges    :: [Edge]
            , directed :: Directed
            , weighted :: Weighted
            } deriving (Eq, Show, Generic, NFData)
            
----- Graph Graphical -----            
data Color = Red
           | Blue
           | Green
           | Purple
           | Grey
           | Yellow
           | Orange
           | Black
           | White
           deriving (Eq, Show, Generic, NFData)

data Thickness = Thin
               | Thick
               deriving (Eq, Show, Generic, NFData)

data Directed  = Directed
               | Undirected
                deriving (Eq, Show, Generic, NFData)
               
data Weighted  = Weighted
               | Unweighted
               deriving (Eq, Show, Generic, NFData)