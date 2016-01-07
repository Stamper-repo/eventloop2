module Eventloop.Utility.Websockets
    ( module Eventloop.Utility.Websockets
    , Connection
    ) where

import qualified Network.Socket as S
import qualified Data.Text as T
import Network.WebSockets hiding (Message)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.SafePrint
import Control.Concurrent.Thread
import Control.Exception
import Data.ByteString.Lazy



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
                                        

handleCloseRequestException :: ClientSocket -> SafePrintToken -> ConnectionException -> IO (Maybe Message)
handleCloseRequestException clientSocket safePrintToken (CloseRequest i reason)
    | i == 1000 = do
                    safePrintLn safePrintToken "Client connection was closed elegantly."
                    S.sClose clientSocket
                    return Nothing
    | otherwise = do
                    safePrintLn safePrintToken ("Connection was closed but reason unknown: " ++ show i ++ " " ++ show reason)
                    S.sClose clientSocket
                    return Nothing
    
handleCloseRequestException clientSocket safePrintToken (ConnectionClosed)
    = do
        safePrintLn safePrintToken ("Connection was closed unexpectedly")
        S.sClose clientSocket
        throw ConnectionClosed
    
handleCloseRequestException clientSocket safePrintToken (ParseException text)
    = do
        safePrintLn safePrintToken ("Parse exception on message: " ++ text)
        S.sClose clientSocket
        throw (ParseException text)
                            

takeMessage :: SafePrintToken -> ClientSocket -> Connection -> IO (Maybe Message)
takeMessage safePrintToken sock conn
    = handle (handleCloseRequestException sock safePrintToken) $ do
        textMessage <- receiveData conn
        let
            message = T.unpack textMessage
        return (Just message)
                                
                                
writeMessage :: Connection -> Message -> IO ()
writeMessage conn message = do
                                sendTextData conn (T.pack message)

writeBinaryMessage :: Connection -> ByteString -> IO ()
writeBinaryMessage conn message = sendBinaryData conn message


isConnected :: S.Socket -> IO Bool
isConnected sock = S.sIsConnected sock

closeWebsocketConnection :: SafePrintToken -> ServerSocket -> ClientSocket -> Connection -> IO ()
closeWebsocketConnection safePrintToken serverSocket clientSocket clientConnection
    = do
        S.sClose serverSocket
        isConnected <- isConnected clientSocket
        case isConnected of
            False -> safePrintLn safePrintToken "Tried to close client connection but was already closed"
            True  -> do
                        safePrintLn safePrintToken "Closing client connection..."
                        handle
                            (\(exception) ->
                                case (exception :: ConnectionException) of
                                    ConnectionClosed -> do
                                                            safePrintLn safePrintToken "Socket still open, but stream had an error"
                                                            S.sClose clientSocket
                                                            safePrintLn safePrintToken "Client connection closed!"
                                    _ -> safePrintLn safePrintToken "this should never happen, contact an administrator! ERROR: WS 01"
                            )
                            ( sendClose clientConnection (T.pack "Shutting down..")
                            )


destroyWebsocketConnection :: ServerSocket -> ClientSocket -> IO ()
destroyWebsocketConnection serverSocket clientSocket
    = do
        S.close serverSocket
        S.close clientSocket
                                                            