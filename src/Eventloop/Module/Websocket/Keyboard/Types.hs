{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.Websocket.Keyboard.Types where

import GHC.Generics (Generic)
import Control.DeepSeq

data Keyboard = Key [Char]
                deriving (Eq, Show, Generic, NFData)