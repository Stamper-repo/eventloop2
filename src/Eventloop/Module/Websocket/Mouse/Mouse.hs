{-# LANGUAGE OverloadedStrings #-}
module Eventloop.Module.Websocket.Mouse.Mouse
    ( setupMouseModuleConfiguration
    , mouseModuleIdentifier
    , mouseInitializer
    , mouseEventRetriever
    , mouseTeardown
    ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.SafePrint
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe

import Eventloop.Module.Websocket.Mouse.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.Utility.Websockets
import Eventloop.Utility.Config
import Eventloop.Utility.Vectors

setupMouseModuleConfiguration :: EventloopSetupModuleConfiguration
setupMouseModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                    mouseModuleIdentifier
                                    (Just mouseInitializer)
                                    (Just mouseEventRetriever)
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just mouseTeardown)
                                  )
     

mouseModuleIdentifier :: EventloopModuleIdentifier
mouseModuleIdentifier = "mouse"


instance FromJSON MouseIn where
    parseJSON vO@(Object v)
        = do
            mouseEvent <- parseJSON vO :: Parser MouseEvent
            mouseType  <- parseJSON vO :: Parser MouseType
            point      <- parseJSON vO :: Parser Point
            id         <- v .: "id"    :: Parser NumericId
            return $ Mouse mouseType id mouseEvent point
    
    
instance FromJSON MouseType where
    parseJSON (Object v)
        = do
            type' <- v .: "elementType" :: Parser String
            return $ case type' of "canvas" -> MouseCanvas
                                   "svg"    -> MouseSVG
    
instance FromJSON MouseEvent where
    parseJSON (Object v)
        =   do
                eventType <- v .: "mouseEventType" :: Parser String
                button <- parseJSON (Object v) :: Parser MouseButton
                return $ case eventType of "click"      -> Click button
                                           "dblclick"   -> DoubleClick button
                                           "mousedown"  -> MouseDown button
                                           "mouseup"    -> MouseUp button
                                           "mouseenter" -> MouseEnter
                                           "mouseleave" -> MouseLeave
                                           "mousemove"  -> MouseMove
   
instance FromJSON MouseButton where
    parseJSON (Object v)
        = do
            button <- v .: "button" :: Parser String
            return $ case button of "left"   -> MouseLeft
                                    "middle" -> MouseMiddle
                                    "right"  -> MouseRight

instance FromJSON Point where
    parseJSON (Object v)
        = do
            x <- v .: "x"
            y <- v .: "y"
            return $ Point (x, y)
    
    
mouseInitializer :: Initializer
mouseInitializer sharedConst sharedIO
    = do
        (clientSocket, clientConn, serverSock) <- setupWebsocketConnection ipAddress  mousePort
        safePrintLn (safePrintToken sharedConst) "Mouse connection succesfull"
        return (sharedConst, sharedIO, MouseConstants clientSocket clientConn serverSock, NoState)


mouseEventRetriever :: EventRetriever
mouseEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        messageM <- takeMessage sock conn
        case messageM of
            Nothing        -> return []
            (Just message) -> return [InMouse $ messageToMouseIn message]
    where
        sock = clientSocket ioConst
        conn = clientConnection ioConst


messageToMouseIn :: Message -> MouseIn
messageToMouseIn message = fromJust.decode $ BL.pack message


mouseTeardown :: Teardown
mouseTeardown sharedConst sharedIO ioConst ioStateT
    = do
        closeWebsocketConnection serverSock clientSock conn
        return sharedIO
    where
        serverSock = serverSocket ioConst
        clientSock = clientSocket ioConst
        conn = clientConnection ioConst
