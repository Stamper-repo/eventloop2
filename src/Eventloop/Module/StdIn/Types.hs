{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.StdIn.Types where

import GHC.Generics (Generic)
import Control.DeepSeq

data StdInIn = StdInReceivedContents [[Char]]
             | StdInReceivedLine [Char]
             | StdInReceivedChar Char
             deriving (Eq, Show)

data StdInOut = StdInReceiveContents
              | StdInReceiveLine
              | StdInReceiveChar
              deriving (Eq, Show, Generic, NFData)