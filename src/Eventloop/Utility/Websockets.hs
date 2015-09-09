module Eventloop.Utility.Websockets
    ( module Eventloop.Utility.Websockets
    , Connection
    ) where

import qualified Network.Socket as S
import qualified Data.Text as T
import Network.WebSockets hiding (Message)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Data.ByteString.Lazy

import Control.Concurrent.Thread

type Host = [Char]
type Port = Int
type Message = [Char]
type ReceiveBuffer = MVar Message

type ServerSocket = S.Socket
type ClientSocket = S.Socket

type ReaderThread = Thread
type UnbufferedReaderThread = ReaderThread

                                    
createBindListenServerSocket :: Host -> Port -> IO ServerSocket
createBindListenServerSocket host port = do
                                        host' <- S.inet_addr host
                                        socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
                                        S.setSocketOption socket S.ReuseAddr 1
                                        S.bindSocket socket (S.SockAddrInet (fromIntegral port) host')
                                        S.listen socket 5
                                        return socket

                                        
acceptFirstConnection :: S.Socket -> IO (Connection, ClientSocket)
acceptFirstConnection serverSocket = do
                                        (clientSocket, clientAddr) <- S.accept serverSocket
                                        pendingConnection <- makePendingConnection clientSocket defaultConnectionOptions
                                        connection <- acceptRequest pendingConnection
                                        return (connection, clientSocket)

                                        
setupWebsocketConnection :: Host -> Port -> IO (ReceiveBuffer, ClientSocket, Connection, ServerSocket, UnbufferedReaderThread)
setupWebsocketConnection host port = S.withSocketsDo $ do
                                                        serverSocket <- createBindListenServerSocket host port
                                                        (clientConnection, clientSocket) <- acceptFirstConnection serverSocket
                                                        recvBuffer <- newEmptyMVar
                                                        readerThread <- spawnUnbufferedReader recvBuffer clientConnection clientSocket
                                                        return (recvBuffer, clientSocket, clientConnection, serverSocket, readerThread)
                                        
                                        
spawnUnbufferedReader :: ReceiveBuffer -> Connection -> ClientSocket -> IO UnbufferedReaderThread
spawnUnbufferedReader recvBuffer conn clientSocket 
    = forkThread (handle (handleCloseRequestException clientSocket) $ readIntoBuffer recvBuffer conn)
                                
                                
readIntoBuffer :: ReceiveBuffer -> Connection -> IO ()
readIntoBuffer recvBuffer conn =  do
                                    textMessage <- receiveData conn
                                    let
                                        message = T.unpack textMessage
                                    putMVar recvBuffer message
                                    readIntoBuffer recvBuffer conn
 
 
handleCloseRequestException :: ClientSocket -> ConnectionException -> IO ()
handleCloseRequestException clientSocket (CloseRequest i reason) 
    | i == 1000 = do
                    Prelude.putStrLn "Client connected was closed elegantly."
                    S.sClose clientSocket
    | otherwise = do
                    Prelude.putStrLn ("Connection was closed but reason unknown: " ++ show i ++ " " ++ show reason)
                    S.sClose clientSocket
    
handleCloseRequestException clientSocket (ConnectionClosed)
    = do
        Prelude.putStrLn ("Connection was closed unexpectedly")
        S.sClose clientSocket
        throw ConnectionClosed
    
handleCloseRequestException clientSocket (ParseException text) 
    = do
        Prelude.putStrLn ("Parse exception on message: " ++ text)
        S.sClose clientSocket
        throw (ParseException text)
                            
                            
hasMessage :: ReceiveBuffer -> IO Bool
hasMessage recvBuffer = do
                            buffer <- tryReadMVar recvBuffer
                            return (buffer /= Nothing)
                            
                            
takeMessage :: ReceiveBuffer -> IO Message
takeMessage recvBuffer = do
                            message <- takeMVar recvBuffer
                            return message
                                
                                
writeMessage :: Connection -> Message -> IO ()
writeMessage conn message = sendTextData conn (T.pack message)
                            

writeBinaryMessage :: Connection -> ByteString -> IO ()
writeBinaryMessage conn message = sendBinaryData conn message


closeWebsocketConnection :: ServerSocket -> ClientSocket -> Connection -> ReaderThread -> IO ()
closeWebsocketConnection serverSocket clientSocket clientConnection readerThread
    = do
        S.sClose serverSocket
        isConnected <- S.sIsConnected clientSocket
        case isConnected of
            False -> Prelude.putStrLn "Tried to close client connection but was already closed"
            True  -> do
                        Prelude.putStrLn "Closing client connection..."
                        handle
                            (\(exception) ->
                                case (exception :: ConnectionException) of
                                    ConnectionClosed -> do
                                                            Prelude.putStrLn "Socket still open, but stream had an error"
                                                            S.sClose clientSocket
                                                            Prelude.putStrLn "Client connection closed!"
                                    _ -> Prelude.putStrLn "this should never happen, contact an administrator! ERROR: WS 01"
                            )
                            ( sendClose clientConnection (T.pack "Shutting down..")
                            )
        joinThread readerThread
        
                                                            