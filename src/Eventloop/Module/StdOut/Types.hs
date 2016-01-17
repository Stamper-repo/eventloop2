{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.StdOut.Types where

import GHC.Generics (Generic)
import Control.DeepSeq

data StdOutOut = StdOutMessage [Char]
            deriving (Eq, Show, Generic, NFData)