module Eventloop.Module.StdOut.StdOut
    ( setupStdOutModuleConfiguration
    , stdOutModuleIdentifier
    , stdOutEventSender
    ) where

import System.IO

import Control.Concurrent.MVar
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
stdOutEventSender sharedConst sharedIOT ioConst ioStateT (OutStdOut (StdOutMessage str))
    = do
        safePrint token str
        hFlush stdout
    where
        token = safePrintToken sharedConst