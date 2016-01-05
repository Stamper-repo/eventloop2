module Eventloop.Module.BasicShapes.BasicShapes
    ( setupBasicShapesModuleConfiguration
    , basicShapesModuleIdentifier
    , basicShapesPostProcessor
    ) where

import Eventloop.Module.BasicShapes.Types
import Eventloop.Module.BasicShapes.Classes
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

setupBasicShapesModuleConfiguration :: EventloopSetupModuleConfiguration
setupBasicShapesModuleConfiguration = ( EventloopSetupModuleConfiguration
                                            basicShapesModuleIdentifier
                                            Nothing
                                            Nothing
                                            Nothing
                                            (Just basicShapesPostProcessor)
                                            Nothing
                                            Nothing
                                        )


basicShapesModuleIdentifier :: EventloopModuleIdentifier
basicShapesModuleIdentifier = "basicshapes"


basicShapesPostProcessor :: PostProcessor
basicShapesPostProcessor sharedConst sharedIOT ioConst ioStateT (OutBasicShapes basicShapesOut)
    = return [OutCanvas canvasOut]
    where
        canvasOut = toCanvasOut basicShapesOut
        
basicShapesPostProcessor sharedConst sharedIOT ioConst ioStateT out
    = return [out]