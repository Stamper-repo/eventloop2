module Eventloop.Module.Timer.Types where
    
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
    
type MicroSecondDelay = Int -- Microseconds
type TimerId = [Char]
type TickBuffer = BlockingConcurrentQueue TimerIn
type StartedTimer = (TimerId, TimerIO)
type TimerStartFunction = (TimerIO -> IO () -> Delay -> IO Bool)

data TimerIn = Tick TimerId
             deriving (Eq, Show)
             
data TimerOut = SetTimer TimerId MicroSecondDelay
              | SetIntervalTimer TimerId MicroSecondDelay
              | UnsetTimer TimerId
              deriving (Eq, Show)