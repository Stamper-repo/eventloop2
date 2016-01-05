module Eventloop.Types.System where

import Control.Concurrent
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.SafePrint
import Control.Concurrent.STM
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Data.Maybe

import Eventloop.Module.Websocket.Keyboard.Types
import Eventloop.Module.Websocket.Mouse.Types
import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.File.Types
import Eventloop.Module.StatefulGraphics.Types
import Eventloop.Module.StdIn.Types
import Eventloop.Module.StdOut.Types
import Eventloop.Module.Timer.Types

import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.Exception
import Eventloop.Utility.Websockets


type Initializer = SharedIOConstants -> SharedIOState -> IO (SharedIOConstants, SharedIOState, IOConstants, IOState)
type EventRetriever = SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> IO [In]
type PreProcessor = SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> In -> IO [In]
type PostProcessor = SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> Out -> IO [Out]
type EventSender = SharedIOConstants -> TVar SharedIOState -> IOConstants -> TVar IOState -> Out -> IO ()
type Teardown = SharedIOConstants -> SharedIOState -> IOConstants -> IOState -> IO SharedIOState

type OutEventRouter = Out -> EventloopModuleIdentifier

type InEventQueue = BlockingConcurrentQueue In
type OutEventQueue = BlockingConcurrentQueue Out
type SenderEventQueue = BlockingConcurrentQueue Out

-- System Configurations
data EventloopModuleConfiguration
    = EventloopModuleConfiguration { moduleId :: EventloopModuleIdentifier
                                   , ioConstants :: IOConstants
                                   , ioStateT :: TVar IOState
                                   , initializerM :: Maybe Initializer
                                   , retrieverM :: Maybe EventRetriever
                                   , preprocessorM :: Maybe PreProcessor
                                   , postprocessorM :: Maybe PostProcessor
                                   , senderConfigM :: Maybe EventloopModuleSenderConfiguration
                                   , teardownM :: Maybe Teardown
                                   }
    
    
data EventloopModuleSenderConfiguration 
    = EventloopModuleSenderConfiguration { sender :: EventSender
                                         , senderEventQueue :: BlockingConcurrentQueue Out
                                         }

                                         
data EventloopConfiguration progstateT
    = EventloopConfiguration { progstateT :: TVar progstateT
                             , eventloopFunc :: progstateT -> In -> (progstateT, [Out])
                             , inEventQueue :: InEventQueue
                             , outEventQueue :: OutEventQueue
                             }
                               

data EventloopSystemConfiguration progstateT
    = EventloopSystemConfiguration { eventloopConfig :: EventloopConfiguration progstateT
                                   , moduleConfigs :: [EventloopModuleConfiguration]
                                   , sharedIOConstants :: SharedIOConstants
                                   , sharedIOStateT :: TVar SharedIOState
                                   , systemThreadId :: ThreadId
                                   , retrieverThreadsM :: MVar [Thread]
                                   , outRouterThreadM :: MVar Thread
                                   , senderThreadsM :: MVar [Thread]
                                   , exceptions :: ExceptionCollection EventloopException
                                   , isStoppingM :: MVar Bool
                                   }
                        
-- Setup Configurations
data EventloopSetupConfiguration progstateT
    = EventloopSetupConfiguration { beginProgstate :: progstateT
                                  , eventloopF :: progstateT -> In -> (progstateT, [Out])
                                  , setupModuleConfigurations :: [EventloopSetupModuleConfiguration]
                                  }
         
         
data EventloopSetupModuleConfiguration
    = EventloopSetupModuleConfiguration { moduleIdentifier :: EventloopModuleIdentifier
                                        , initializerF :: Maybe Initializer
                                        , eventRetrieverF :: Maybe EventRetriever
                                        , preprocessorF :: Maybe PreProcessor
                                        , postprocessorF :: Maybe PostProcessor
                                        , eventSenderF :: Maybe EventSender
                                        , teardownF :: Maybe Teardown
                                        }
         
         
-- Shared IO State
data SharedIOConstants = SharedIOConstants { safePrintToken :: SafePrintToken
                                           , measureText :: CanvasText -> IO ScreenDimensions
                                           }
data SharedIOState = SharedIOState {}
         
-- Modules IO State
data IOConstants = MouseConstants { clientSocket         :: ClientSocket
                                  , clientConnection     :: Connection
                                  , serverSocket         :: ServerSocket
                                  }
                 | KeyboardConstants { clientSocket     :: ClientSocket
                                     , clientConnection :: Connection
                                     , serverSocket     :: ServerSocket
                                     }
                 | CanvasConstants { canvasSystemReceiveBuffer :: CanvasSystemReceiveBuffer
                                   , clientSocket              :: ClientSocket
                                   , clientConnection          :: Connection
                                   , serverSocket              :: ServerSocket
                                   }
                 | StdInConstants { stdInInQueue :: BlockingConcurrentQueue StdInIn
                                  }
                 | TimerConstants { tickInQueue :: BlockingConcurrentQueue TimerIn
                                  }
                 | FileConstants { fileInQueue :: BlockingConcurrentQueue FileIn
                                 }
                 | NoConstants
                 deriving Show

data IOState = TimerState { startedIntervalTimers      :: [StartedTimer]
                          , startedTimers              :: [StartedTimer]
                          }
             | FileState { opened :: [OpenFile]
                         }
             | StatefulGraphicsState { states :: GraphicsStates
                                     }
             | NoState
