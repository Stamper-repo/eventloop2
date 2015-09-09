 {-# LANGUAGE OverloadedStrings #-}
module Eventloop.Module.Websocket.Keyboard.Keyboard
    ( setupKeyboardModuleConfiguration
    , keyboardModuleIdentifier
    , keyboardInitializer
    , keyboardEventRetriever
    , keyboardTeardown
    ) where

import Data.Aeson
import Data.Maybe
import Control.Applicative
import Control.Concurrent.SafePrint
import qualified Data.ByteString.Lazy.Char8 as BL

import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Module.Websocket.Keyboard.Types
import Eventloop.Utility.Config
import qualified Eventloop.Utility.BufferedWebsockets as WS


setupKeyboardModuleConfiguration :: EventloopSetupModuleConfiguration
setupKeyboardModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                        keyboardModuleIdentifier
                                        (Just keyboardInitializer)
                                        (Just keyboardEventRetriever)
                                        Nothing
                                        Nothing
                                        Nothing
                                        (Just keyboardTeardown)
                                      )

                                      
keyboardModuleIdentifier :: EventloopModuleIdentifier
keyboardModuleIdentifier = "keyboard"

                  
instance FromJSON Keyboard where
    parseJSON (Object v) = Key <$> v .: "key"

    
keyboardInitializer :: Initializer
keyboardInitializer sharedIO
    = do
        (recvBuffer, clientSocket, clientConn, serverSock, bufferedReaderThread) <- WS.setupWebsocketConnection ipAddress  keyboardPort
        safePrintLn (safePrintToken sharedIO) "Keyboard connection successfull"
        return (sharedIO, KeyboardState recvBuffer clientSocket clientConn serverSock bufferedReaderThread)

                            
keyboardEventRetriever :: EventRetriever
keyboardEventRetriever sharedIO keyboardState = do
                                                    messages <- WS.takeMessages (receiveBuffer keyboardState)
                                                    return (sharedIO, keyboardState, map ((.) InKeyboard messageToKeyboardIn) messages)
                                        

messageToKeyboardIn :: WS.Message -> Keyboard
messageToKeyboardIn message = fromJust.decode $ BL.pack message


keyboardTeardown :: Teardown
keyboardTeardown sharedIO ks
    = do
        WS.closeWebsocketConnection (serverSocket ks) (clientSocket ks) (clientConnection ks) (bufferedReaderThread ks)
        return sharedIO




