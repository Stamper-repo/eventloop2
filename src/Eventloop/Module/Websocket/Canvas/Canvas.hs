module Eventloop.Module.Websocket.Canvas.Canvas
    ( setupCanvasModuleConfiguration
    , canvasModuleIdentifier
    , canvasInitializer
    , canvasEventRetriever
    , canvasEventSender
    ) where

    
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.SafePrint
import Control.Concurrent.Thread
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS

import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.Websocket.Canvas.JSONEncoding
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Config
import Eventloop.Utility.Websockets


setupCanvasModuleConfiguration :: EventloopSetupModuleConfiguration
setupCanvasModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                        canvasModuleIdentifier
                                        (Just canvasInitializer)
                                        (Just canvasEventRetriever)
                                        Nothing
                                        Nothing
                                        (Just canvasEventSender)
                                        (Just canvasTeardown)
                                      )
                                      
                                      
canvasModuleIdentifier :: EventloopModuleIdentifier
canvasModuleIdentifier = "canvas"


canvasInitializer :: Initializer
canvasInitializer sharedConst sharedIO
    = do
        (clientSocket, clientConn, serverSock) <- setupWebsocketConnection ipAddress canvasPort
        safePrintLn (safePrintToken sharedConst) "Canvas connection successfull!"
        sysRecvBuffer <- newEmptyMVar
        measureTextLock <- newMVar ()
        let
            ioConst = CanvasConstants sysRecvBuffer clientSocket clientConn serverSock
            measureText_' = measureText_ ioConst measureTextLock
            sharedConst' = sharedConst{measureText = measureText_'}
        return (sharedConst', sharedIO, ioConst, NoState)


canvasEventRetriever :: EventRetriever
canvasEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        isConnected <- isConnected sock
        case isConnected of
            False -> return []
            True -> do
                messageM <- takeMessage safePrintToken_ sock conn
                case messageM of
                    Nothing -> return []
                    (Just message) -> do
                                        let
                                            inRouted = fromJust.decode $ LBS.pack message
                                        inCanvasM <- route sysBuffer inRouted
                                        case inCanvasM of
                                            Nothing         -> return []
                                            (Just inCanvas) -> return [InCanvas inCanvas]
    where
        sock = clientSocket ioConst
        conn = clientConnection ioConst
        sysBuffer = canvasSystemReceiveBuffer ioConst
        safePrintToken_ = safePrintToken sharedConst


canvasEventSender :: EventSender
canvasEventSender sharedConst sharedIOT ioConst ioStateT (OutCanvas canvasOut)
    = sendRoutedMessageOut conn (OutUserCanvas canvasOut)
    where
        conn = clientConnection ioConst

canvasEventSender sharedConst sharedIOT ioConst ioStateT Stop
    = do
        closeWebsocketConnection safePrintToken_ serverSock clientSock conn
    where
        serverSock = serverSocket ioConst
        clientSock = clientSocket ioConst
        conn = clientConnection ioConst
        safePrintToken_ = safePrintToken sharedConst


canvasTeardown :: Teardown
canvasTeardown sharedConst sharedIO ioConst ioState
    = do
        destroyWebsocketConnection serverSock clientSock
        return sharedIO
    where
        serverSock = serverSocket ioConst
        clientSock = clientSocket ioConst
        conn = clientConnection ioConst


sendRoutedMessageOut :: Connection -> RoutedMessageOut -> IO ()
sendRoutedMessageOut conn out = writeMessage conn $ LBS.unpack $ encode out


route :: CanvasSystemReceiveBuffer -> RoutedMessageIn -> IO (Maybe CanvasIn)
route sysRecvBuffer routedIn
    = case routedIn of
        (InUserCanvas canvasIn)   -> return (Just canvasIn)
        (InSystemCanvas canvasIn) -> do
                                        putMVar sysRecvBuffer canvasIn
                                        return Nothing


measureText_ :: IOConstants -> MVar () -> CanvasText -> IO ScreenDimensions
measureText_ ioConst lock canvasText
    = do
        lock_ <- takeMVar lock
        sendRoutedMessageOut conn outMsg
        (SystemMeasuredText _ screenDims) <- takeMVar buf
        putMVar lock lock_
        return screenDims
    where
        conn = clientConnection ioConst
        buf = canvasSystemReceiveBuffer ioConst
        outMsg = OutSystemCanvas $ SystemMeasureText canvasText