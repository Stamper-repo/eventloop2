module Eventloop.Types.System where

import Control.Concurrent
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.SafePrint
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Data.Maybe

import Eventloop.Module.Websocket.Keyboard.Types
import Eventloop.Module.Websocket.Mouse.Types
import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.File.Types
import Eventloop.Module.StdIn.Types
import Eventloop.Module.StdOut.Types
import Eventloop.Module.Timer.Types

import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.Exception
import qualified Eventloop.Utility.Websockets as WS
import qualified Eventloop.Utility.BufferedWebsockets as BWS


type Initializer = SharedIOState -> IO (SharedIOState, IOState)
type EventRetriever = SharedIOState -> IOState -> IO (SharedIOState, IOState, [In])
type PreProcessor = SharedIOState -> IOState -> In -> IO (SharedIOState, IOState, [In])
type PostProcessor = SharedIOState -> IOState -> Out -> IO (SharedIOState, IOState, [Out])
type EventSender = SharedIOState -> IOState -> Out -> IO (SharedIOState, IOState)
type Teardown = SharedIOState -> IOState -> IO SharedIOState

type OutEventRouter = Out -> EventloopModuleIdentifier

type InEventQueue = BlockingConcurrentQueue In
type OutEventQueue = BlockingConcurrentQueue Out
type SenderEventQueue = BlockingConcurrentQueue Out

-- System Configurations
data EventloopModuleConfiguration
    = EventloopModuleConfiguration { moduleId :: EventloopModuleIdentifier
                                   , iostateM :: MVar IOState
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
    = EventloopConfiguration { progstateM :: MVar progstateT
                             , eventloopFunc :: progstateT -> In -> (progstateT, [Out])
                             , inEventQueue :: InEventQueue
                             , outEventQueue :: OutEventQueue
                             }
                               

data EventloopSystemConfiguration progstateT
    = EventloopSystemConfiguration { eventloopConfig :: EventloopConfiguration progstateT
                                   , moduleConfigs :: [EventloopModuleConfiguration]
                                   , sharedIOStateM :: MVar SharedIOState
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
data SharedIOState = SharedIOState { safePrintToken :: SafePrintToken
                                   , measureText :: CanvasText -> IO ScreenDimensions
                                   }
         
-- Modules IO State
data IOState = MouseState { receiveBuffer        :: BWS.BufferedReceiveBuffer
                          , clientSocket         :: BWS.ClientSocket
                          , clientConnection     :: BWS.Connection
                          , serverSocket         :: BWS.ServerSocket
                          , bufferedReaderThread :: BWS.BufferedReaderThread
                          }
             | KeyboardState { receiveBuffer    :: BWS.BufferedReceiveBuffer
                             , clientSocket     :: BWS.ClientSocket
                             , clientConnection :: BWS.Connection
                             , serverSocket     :: BWS.ServerSocket
                             , bufferedReaderThread :: BWS.BufferedReaderThread
                             }
             | CanvasState { commonReceiveBuffer       :: WS.ReceiveBuffer
                           , canvasUserReceiveBuffer   :: CanvasUserReceiveBuffer
                           , canvasSystemReceiveBuffer :: CanvasSystemReceiveBuffer
                           , clientSocket              :: WS.ClientSocket
                           , clientConnection          :: WS.Connection
                           , serverSocket              :: WS.ServerSocket
                           , unbufferedReaderThread    :: WS.UnbufferedReaderThread
                           , routerThread              :: Thread
                           }
             | StdInState { newStdInInEvents :: [StdInIn] 
                          }
             | TimerState { startedIntervalTimers      :: [StartedTimer]
                          , startedTimers              :: [StartedTimer]
                          , incomingIntervalTickBuffer :: IncomingTickBuffer
                          , incomingTickBuffer         :: IncomingTickBuffer
                          }
             | FileState { newFileInEvents :: [FileIn]
                         , opened          :: [OpenFile]
                         }
             | NoState