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
basicShapesPostProcessor shared iostate (OutBasicShapes basicShapesOut)
    = return (shared, iostate, [OutCanvas canvasOut])
    where
        canvasOut = toCanvasOut basicShapesOut
        
basicShapesPostProcessor shared iostate out = return (shared, iostate, [out])