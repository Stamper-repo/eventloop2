module Eventloop.Module.StdIn.StdIn
    ( setupStdInModuleConfiguration
    , stdInModuleIdentifier
    , stdInInitializer
    , stdInEventRetriever
    , stdInEventSender
    ) where

import System.IO
import Data.String
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Control.Concurrent.STM

import Eventloop.Module.StdIn.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

setupStdInModuleConfiguration :: EventloopSetupModuleConfiguration
setupStdInModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                    stdInModuleIdentifier
                                    (Just stdInInitializer)
                                    (Just stdInEventRetriever)
                                    Nothing
                                    Nothing
                                    (Just stdInEventSender)
                                    Nothing
                                  )
                                  

stdInModuleIdentifier :: EventloopModuleIdentifier
stdInModuleIdentifier = "stdin"


stdInInitializer :: Initializer
stdInInitializer sharedConst sharedIO
    = do
        inQueue <- createBlockingConcurrentQueue
        return (sharedConst, sharedIO, StdInConstants inQueue, NoState)

    
stdInEventRetriever :: EventRetriever
stdInEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        inEvents <- takeAllFromBlockingConcurrentQueue inQueue
        return (map InStdIn inEvents)
    where
        inQueue = stdInInQueue ioConst

        
stdInEventSender :: EventSender
stdInEventSender sharedConst sharedIOT ioConst ioStateT (OutStdIn a)
    = do
        inEvents <- stdInEventSender' a
        putAllInBlockingConcurrentQueue inQueue inEvents
    where
        inQueue = stdInInQueue ioConst
        
        
stdInEventSender' :: StdInOut -> IO [StdInIn]
stdInEventSender' StdInReceiveContents
    = doStdInGet linedGetContents StdInReceivedContents
    where
        linedGetContents = (getContents >>= (\strContents -> return $ lines strContents))
                  
stdInEventSender' StdInReceiveLine
    = doStdInGet getLine StdInReceivedLine
stdInEventSender' StdInReceiveChar
    = doStdInGet getChar StdInReceivedChar
                                                
                                                
doStdInGet :: (IO a) -> (a -> StdInIn) -> IO [StdInIn]
doStdInGet source typeEvent
    = do
        content <- source
        return ([typeEvent content])