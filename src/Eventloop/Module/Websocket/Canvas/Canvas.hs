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
import Control.Concurrent.Thread
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS

import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.Websocket.Canvas.JSONEncoding
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Config
import qualified Eventloop.Utility.Websockets as WS


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
canvasInitializer sharedIO
    = do
        (comRecvBuffer, clientSocket, clientConn, serverSock, unbufferedReaderThread) <- WS.setupWebsocketConnection ipAddress canvasPort
        putStrLn "Canvas connection successfull!"
        userRecvBuffer <- newMVar []
        sysRecvBuffer <- newEmptyMVar
        routerThread <- forkThread (router comRecvBuffer userRecvBuffer sysRecvBuffer)
        --TODO Add measuretext to sharedIO
        return (sharedIO, CanvasState comRecvBuffer userRecvBuffer sysRecvBuffer clientSocket clientConn serverSock unbufferedReaderThread routerThread)


canvasEventRetriever :: EventRetriever
canvasEventRetriever sharedIO canvasState = do
                                                let
                                                    userRecvBuffer = canvasUserReceiveBuffer canvasState
                                                messages <- takeMVar userRecvBuffer
                                                putMVar userRecvBuffer []
                                                return (sharedIO, canvasState, (map InCanvas messages))

                                    
canvasEventSender :: EventSender
canvasEventSender sharedIO canvasState (OutCanvas canvasOut) = do
                                                        let
                                                          conn = clientConnection canvasState  
                                                        sendRoutedMessageOut conn (OutUserCanvas canvasOut)
                                                        return (sharedIO, canvasState)
                                    

                                    
canvasTeardown :: Teardown
canvasTeardown sharedIO canvasState
    = do
        WS.closeWebsocketConnection (serverSocket canvasState) (clientSocket canvasState) (clientConnection canvasState) (unbufferedReaderThread canvasState)
        terminateThread (routerThread canvasState)
        joinThread (routerThread canvasState)
        -- Todo teardown measureText websocket connection
        return sharedIO
    
    
sendRoutedMessageOut :: WS.Connection -> RoutedMessageOut -> IO ()
sendRoutedMessageOut conn out = WS.writeMessage conn $ LBS.unpack $ encode out


router :: WS.ReceiveBuffer -> CanvasUserReceiveBuffer -> CanvasSystemReceiveBuffer -> IO ()
router comRecvBuffer userRecvBuffer sysRecvBuffer = do
                                                        encodedRoutedIn <- takeMVar comRecvBuffer
                                                        let
                                                            Just routedIn = decode $ LBS.pack encodedRoutedIn :: Maybe RoutedMessageIn
                                                        case routedIn of
                                                            (InUserCanvas canvasIn)   -> do
                                                                                            ins <- takeMVar userRecvBuffer
                                                                                            putMVar userRecvBuffer (ins ++ [canvasIn])
                                                                                            nextStep
                                                            (InSystemCanvas canvasIn) -> do
                                                                                            putMVar sysRecvBuffer canvasIn
                                                                                            nextStep
                                                  where
                                                    nextStep = router comRecvBuffer userRecvBuffer sysRecvBuffer

                                                    
--TODO
measureText :: IOState -> CanvasId -> CanvasText -> IO ScreenDimensions
measureText canvasState canvasId canvasText = return (4,4)
                                                



    
    

