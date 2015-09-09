module Eventloop.Module.StdIn.Types where

data StdInIn = StdInReceivedContents [[Char]]
             | StdInReceivedLine [Char]
             | StdInReceivedChar Char
             deriving (Eq, Show)

data StdInOut = StdInReceiveContents
              | StdInReceiveLine
              | StdInReceiveChar
              deriving (Eq, Show)