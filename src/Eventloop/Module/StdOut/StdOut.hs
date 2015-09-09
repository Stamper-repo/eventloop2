module Eventloop.Module.StdOut.StdOut
    ( setupStdOutModuleConfiguration
    , stdOutModuleIdentifier
    , stdOutEventSender
    ) where

import System.IO

import Control.Concurrent.SafePrint

import Eventloop.Module.StdOut.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

setupStdOutModuleConfiguration :: EventloopSetupModuleConfiguration
setupStdOutModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                    stdOutModuleIdentifier
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just stdOutEventSender)
                                    Nothing
                                  )

stdOutModuleIdentifier :: EventloopModuleIdentifier
stdOutModuleIdentifier = "stdout"


stdOutEventSender :: EventSender
stdOutEventSender sharedIO state (OutStdOut (StdOutMessage str)) = do
                                                                    safePrint (safePrintToken sharedIO) str
                                                                    hFlush stdout
                                                                    return (sharedIO, state)