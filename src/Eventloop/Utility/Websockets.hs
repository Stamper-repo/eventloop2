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

instance Show Connection where
    show _ = "Connection"
                                    
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

                                        
setupWebsocketConnection :: Host -> Port -> IO (ClientSocket, Connection, ServerSocket)
setupWebsocketConnection host port = S.withSocketsDo $ do
                                                        serverSocket <- createBindListenServerSocket host port
                                                        (clientConnection, clientSocket) <- acceptFirstConnection serverSocket
                                                        return (clientSocket, clientConnection, serverSocket)
                                        

handleCloseRequestException :: ClientSocket -> ConnectionException -> IO (Maybe Message)
handleCloseRequestException clientSocket (CloseRequest i reason) 
    | i == 1000 = do
                    Prelude.putStrLn "Client connected was closed elegantly."
                    S.sClose clientSocket
                    return Nothing
    | otherwise = do
                    Prelude.putStrLn ("Connection was closed but reason unknown: " ++ show i ++ " " ++ show reason)
                    S.sClose clientSocket
                    return Nothing
    
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
                            

takeMessage :: ClientSocket -> Connection -> IO (Maybe Message)
takeMessage sock conn
    = handle (handleCloseRequestException sock) $ do
        textMessage <- receiveData conn
        let
            message = T.unpack textMessage
        return (Just message)
                                
                                
writeMessage :: Connection -> Message -> IO ()
writeMessage conn message = do
                                sendTextData conn (T.pack message)

writeBinaryMessage :: Connection -> ByteString -> IO ()
writeBinaryMessage conn message = sendBinaryData conn message


closeWebsocketConnection :: ServerSocket -> ClientSocket -> Connection -> IO ()
closeWebsocketConnection serverSocket clientSocket clientConnection
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
        
                                                            