module TestSinusPoly where


import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes

import Eventloop.Utility.Vectors

xValues :: Float -> Float -> Int -> [Float]
xValues minX maxX amount
    = take amount $ iterate (\x -> x + step) minX
    where
        diff = maxX - minX
        step = diff / (fromIntegral amount)

minX :: Float
minX = 0

maxX :: Float
maxX = 4 * pi

amount :: Int
amount = 100000

sinusZip :: [(Float, Float)]
sinusZip
    = map (\x -> (scaleX' x, (scaleY'.sin) x)) allX
    where
        allX = xValues minX maxX amount
        scaleX' = scale (minX, maxX) (0, fromIntegral $ fst canvasDimensions)
        scaleY' = scale (-1, 1) (fromIntegral $ snd canvasDimensions, 0)

scale :: (Float, Float) -> (Float, Float) -> Float -> Float
scale (domainStart, domainEnd) (rangeStart, rangeEnd) domainV
    = vRangeNorm + rangeStart
    where
        vNorm = domainV - domainStart
        ratio = (rangeEnd - rangeStart) / (domainEnd - domainStart)
        vRangeNorm = vNorm * ratio



data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

canvasDimensions = (512, 512)

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupBasicShapesModuleConfiguration
                                                                   , C.setupCanvasModuleConfiguration
                                                                   ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop


eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ C.SetupCanvas 1 1 canvasDimensions (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                          , OutBasicShapes $ DrawShapes 1 shapes
                          --, OutCanvas $ TeardownCanvas 1
                          , Stop
                          ])
                  where
                    shapes = [ MultiLine points 1 (0,0,0,255) Nothing]
                    points = map Point sinusZip

start = startEventloopSystem eventloopConfiguration