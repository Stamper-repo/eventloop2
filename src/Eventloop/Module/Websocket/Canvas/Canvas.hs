module Eventloop.Module.Websocket.Canvas.Canvas
    ( setupCanvasModuleConfiguration
    , canvasModuleIdentifier
    , canvasInitializer
    , canvasEventRetriever
    , canvasEventSender
    , canvasTeardown
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
        --TODO Add measuretext to sharedIO
        return (sharedConst, sharedIO, CanvasConstants sysRecvBuffer clientSocket clientConn serverSock, NoState)

{-
TODO:
- Bug cleanly disconnect websocket connection?
- measuretext in sharedIO
-}
canvasEventRetriever :: EventRetriever
canvasEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        messageM <- takeMessage sock conn
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


canvasEventSender :: EventSender
canvasEventSender sharedConst sharedIOT ioConst ioStateT (OutCanvas canvasOut)
    = sendRoutedMessageOut conn (OutUserCanvas canvasOut)
    where
        conn = clientConnection ioConst
                                    

canvasTeardown :: Teardown
canvasTeardown sharedConst sharedIO ioConst ioState
    = do
        closeWebsocketConnection serverSock clientSock conn
        -- Todo teardown measureText websocket connection
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

                                                    
--TODO
measureText :: IOState -> CanvasId -> CanvasText -> IO ScreenDimensions
measureText canvasState canvasId canvasText = return (4,4)
