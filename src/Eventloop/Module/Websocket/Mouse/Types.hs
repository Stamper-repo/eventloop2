{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.Websocket.Mouse.Types where

import GHC.Generics (Generic)
import Control.DeepSeq

import Eventloop.Types.Common
import Eventloop.Utility.Vectors

data MouseIn = Mouse MouseType NumericId MouseEvent Point
             deriving (Eq, Show)

data MouseType = MouseCanvas
               | MouseSVG
               deriving (Eq, Show)

data MouseEvent = Click MouseButton
                | DoubleClick MouseButton
                | MouseMove
                | MouseDown MouseButton
                | MouseUp MouseButton
                | MouseEnter
                | MouseLeave
                deriving (Eq, Show)
         
data MouseButton = MouseLeft 
                 | MouseRight 
                 | MouseMiddle
                 deriving (Eq, Show, Generic, NFData)