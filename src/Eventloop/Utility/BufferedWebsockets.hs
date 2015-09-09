module Eventloop.Utility.BufferedWebsockets
    ( module Eventloop.Utility.Websockets
    , module Eventloop.Utility.BufferedWebsockets
    ) where

    

import qualified Data.Text as T
import qualified Network.Socket as S
import Network.WebSockets hiding (Message)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception

import Control.Concurrent.Thread
    
import Eventloop.Utility.Websockets hiding ( ReceiveBuffer
                                           , setupWebsocketConnection
                                           , spawnReader
                                           , readIntoBuffer
                                           , hasMessage
                                           , takeMessage
                                           )

type BufferedReceiveBuffer = MVar [Message]
type BufferedReaderThread = ReaderThread

setupWebsocketConnection :: Host -> Port -> IO (BufferedReceiveBuffer, ClientSocket, Connection, ServerSocket, ReaderThread)
setupWebsocketConnection host port 
    = do
        serverSocket <- createBindListenServerSocket host port
        (clientConnection, clientSocket) <- acceptFirstConnection serverSocket
        recvBuffer <- newMVar []
        readerThread <- spawnBufferedReader recvBuffer clientConnection clientSocket
        return (recvBuffer, clientSocket, clientConnection, serverSocket, readerThread)
                                                        
                                                        
spawnBufferedReader :: BufferedReceiveBuffer -> Connection -> ClientSocket -> IO BufferedReaderThread
spawnBufferedReader recvBuffer conn clientSocket 
    = forkThread (handle (handleCloseRequestException clientSocket) $ bufferedReadIntoBuffer recvBuffer conn)
                                                        
                                                        
bufferedReadIntoBuffer :: BufferedReceiveBuffer -> Connection -> IO ()
bufferedReadIntoBuffer recvBuffer conn = do
                                    textMessage <- receiveData conn
                                    let
                                        message = T.unpack textMessage
                                    messages <- takeMVar recvBuffer
                                    putMVar recvBuffer (messages ++ [message])
                                    bufferedReadIntoBuffer recvBuffer conn
                                    
                                    
hasMessages :: BufferedReceiveBuffer -> IO Bool
hasMessages recvBuffer = do
                            messages <- readMVar recvBuffer
                            return ((length messages) > 0)
                            
takeMessages :: BufferedReceiveBuffer -> IO [Message]
takeMessages recvBuffer = do
                            messages <- takeMVar recvBuffer
                            putMVar recvBuffer []
                            return messages