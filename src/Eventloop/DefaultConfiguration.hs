module Eventloop.DefaultConfiguration where

import Control.Concurrent.SafePrint

import Eventloop.Module.Websocket.Keyboard
import Eventloop.Module.Websocket.Mouse
import Eventloop.Module.File
import Eventloop.Module.StdOut
import Eventloop.Module.StdIn
import Eventloop.Module.Timer
import Eventloop.Types.Events
import Eventloop.Types.System

allModulesEventloopSetupConfiguration :: progstateT -> {- Begin program state -}
                                         (progstateT -> In -> (progstateT, [Out])) -> {- Eventloop function -}
                                         EventloopSetupConfiguration progstateT
allModulesEventloopSetupConfiguration beginProgstate eventloop
    = ( EventloopSetupConfiguration
            -- Begin program state
            beginProgstate
            
            -- Eventloop function
            eventloop
            
            -- All module setupconfigurations
            [ setupFileModuleConfiguration
            , setupTimerModuleConfiguration
            , setupKeyboardModuleConfiguration
            , setupMouseModuleConfiguration
            , setupStdInModuleConfiguration
            , setupStdOutModuleConfiguration
            ]
        )


setupSharedIOConstants :: IO SharedIOConstants
setupSharedIOConstants
    = do
        safePrintToken <- createSafePrintToken
        return (SharedIOConstants safePrintToken undefined)

setupSharedIOState :: IO SharedIOState
setupSharedIOState
    = return SharedIOState