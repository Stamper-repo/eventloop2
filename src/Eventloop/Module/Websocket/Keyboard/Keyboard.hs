 {-# LANGUAGE OverloadedStrings #-}
module Eventloop.Module.Websocket.Keyboard.Keyboard
    ( setupKeyboardModuleConfiguration
    , keyboardModuleIdentifier
    , keyboardInitializer
    , keyboardEventRetriever
    , keyboardEventSender
    ) where

import Data.Aeson
import Data.Maybe
import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.SafePrint
import qualified Data.ByteString.Lazy.Char8 as BL

import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Module.Websocket.Keyboard.Types
import Eventloop.Utility.Config
import Eventloop.Utility.Websockets


setupKeyboardModuleConfiguration :: EventloopSetupModuleConfiguration
setupKeyboardModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                        keyboardModuleIdentifier
                                        (Just keyboardInitializer)
                                        (Just keyboardEventRetriever)
                                        Nothing
                                        Nothing
                                        (Just keyboardEventSender)
                                        Nothing
                                      )

                                      
keyboardModuleIdentifier :: EventloopModuleIdentifier
keyboardModuleIdentifier = "keyboard"

                  
instance FromJSON Keyboard where
    parseJSON (Object v) = Key <$> v .: "key"

    
keyboardInitializer :: Initializer
keyboardInitializer sharedConst sharedIO
    = do
        (clientSocket, clientConn, serverSock) <- setupWebsocketConnection ipAddress  keyboardPort
        safePrintLn (safePrintToken sharedConst) "Keyboard connection successfull"
        return (sharedConst, sharedIO, KeyboardConstants clientSocket clientConn serverSock, NoState)

                            
keyboardEventRetriever :: EventRetriever
keyboardEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        isConnected <- isConnected sock
        case isConnected of
            False -> return []
            True -> do
                messageM <- takeMessage safePrintToken_ sock conn
                case messageM of
                    Nothing -> return []
                    (Just message) -> return [InKeyboard $ messageToKeyboardIn message]
    where
        sock = clientSocket ioConst
        conn = clientConnection ioConst
        safePrintToken_ = safePrintToken sharedConst


messageToKeyboardIn :: Message -> Keyboard
messageToKeyboardIn message = fromJust.decode $ BL.pack message


keyboardEventSender :: EventSender
keyboardEventSender sharedConst sharedIOT ioConst ioStateT Stop
    = do
        closeWebsocketConnection safePrintToken_ serverSock clientSock conn
    where
        serverSock = serverSocket ioConst
        clientSock = clientSocket ioConst
        conn = clientConnection ioConst
        safePrintToken_ = safePrintToken sharedConst

keyboardTeardown :: Teardown
keyboardTeardown sharedConst sharedIO ioConst ioState
    = do
        destroyWebsocketConnection serverSock clientSock
        return sharedIO
    where
        serverSock = serverSocket ioConst
        clientSock = clientSocket ioConst
        conn = clientConnection ioConst