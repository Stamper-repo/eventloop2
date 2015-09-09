module Eventloop.Module.StdIn.StdIn
    ( setupStdInModuleConfiguration
    , stdInModuleIdentifier
    , stdInInitializer
    , stdInEventRetriever
    , stdInEventSender
    ) where

import System.IO
import Data.String

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
stdInInitializer sharedIO
    = return (sharedIO, StdInState [])

    
stdInEventRetriever :: EventRetriever
stdInEventRetriever sharedIO (StdInState events)
    = return (sharedIO, StdInState [], inEvents)
    where
        inEvents = map InStdIn events

        
stdInEventSender :: EventSender
stdInEventSender sharedIO stdInState (OutStdIn a)
    = do
        stdInState' <- stdInEventSender' stdInState a
        return (sharedIO, stdInState')
        
        
stdInEventSender' :: IOState -> StdInOut -> IO IOState
stdInEventSender' stdInState StdInReceiveContents
    = doStdInGet stdInState linedGetContents StdInReceivedContents
    where
        linedGetContents = (getContents >>= (\strContents -> return $ lines strContents))
                  
stdInEventSender' stdInState StdInReceiveLine = doStdInGet stdInState getLine StdInReceivedLine
stdInEventSender' stdInState StdInReceiveChar = doStdInGet stdInState getChar StdInReceivedChar
                                                
                                                
doStdInGet :: IOState -> (IO a) -> (a -> StdInIn) -> IO IOState
doStdInGet stdInState source typeEvent = do
                                            let
                                                events = newStdInInEvents stdInState
                                            content <- source
                                            let
                                                event = typeEvent content
                                                events' = events ++ [event]
                                            return (stdInState {newStdInInEvents = events'})