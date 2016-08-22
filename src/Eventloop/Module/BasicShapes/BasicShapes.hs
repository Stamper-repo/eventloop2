module Eventloop.Module.BasicShapes.BasicShapes
    ( setupBasicShapesModuleConfiguration
    , basicShapesModuleIdentifier
    , basicShapesPostProcessor
    ) where

import Control.Concurrent.SafePrint

import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.BasicShapes.Classes
import Eventloop.Module.BasicShapes.MeasureTextHack
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

setupBasicShapesModuleConfiguration :: EventloopSetupModuleConfiguration
setupBasicShapesModuleConfiguration = ( EventloopSetupModuleConfiguration
                                            basicShapesModuleIdentifier
                                            (Just basicShapesInitializer)
                                            Nothing
                                            Nothing
                                            (Just basicShapesPostProcessor)
                                            Nothing
                                            Nothing
                                        )


basicShapesModuleIdentifier :: EventloopModuleIdentifier
basicShapesModuleIdentifier = "basicshapes"


basicShapesInitializer :: Initializer
basicShapesInitializer sharedConst sharedIO
    = do
        saveMeasureText (measureText sharedConst)
        return (sharedConst, sharedIO, NoConstants, NoState)


basicShapesPostProcessor :: PostProcessor
basicShapesPostProcessor sharedConst sharedIOT ioConst ioStateT (OutBasicShapes basicShapesOut)
    = return [OutCanvas canvasOut]
    where
        canvasOut = toCanvasOut basicShapesOut
        
basicShapesPostProcessor sharedConst sharedIOT ioConst ioStateT out
    = return [out]