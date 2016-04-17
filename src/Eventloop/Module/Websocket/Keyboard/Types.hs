{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Eventloop.Module.Websocket.Keyboard.Types where

import GHC.Generics (Generic)
import Control.DeepSeq

{-| Almost all key presses are registered including modifier keys.
 Expect character keys to come in as their character. Press a c, get
 a "c". If a modifier is used and a different character is expected,
 it will be that instead. Press shift + c, get a "C". Modifiers are also
 sent as their string representation: "shift", "ctrl" or "alt". Space is
 expected as "space". -}
data Keyboard = Key [Char]
                deriving (Eq, Show, Generic, NFData)