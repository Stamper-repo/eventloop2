module TestBasicShapesBoundingBox where

import Prelude

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.BasicShapes
import Eventloop.Utility.Vectors

import Debug.Trace

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

canvasDimensions = (512, 512)

eventloopConfiguration = defaultConfig { setupModuleConfigurations=[ setupBasicShapesModuleConfiguration
                                                                   , C.setupCanvasModuleConfiguration
                                                                   ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop


shape prim = prim 5 (0, 0, 0, 255)
        -- (Just (Rotation AroundCenter 35))
         Nothing
        --(Just (Rotation (AroundPoint (Point (256, 256))) 45))


rect = shape $ Rectangle (Point (125, 135)) (52, 10) (255, 0, 0, 255)
circ = shape $ Circle (Point (125, 50)) 25 (0, 255, 0, 255)
text_ = shape $ Text "Hellog World!" "New Times Roman" 24 (Point (200, 25)) AlignLeft (125, 125, 125, 255)
polygon1 = shape $ RegularPolygon (Point (50, 200)) 3 50 (0, 0, 255, 255)
polygon2 = shape $ RegularPolygon (Point (200, 200)) 5 50 (0, 255, 0, 255)
line = shape $ Line (Point (300, 225)) (Point (350, 250))
multiline = shape $ MultiLine [Point (400, 225), Point (425, 250), Point (450, 400)]
multiline2 = MultiLine [Point (400, 225), Point (425, 250), Point (450, 400)] 2 (255, 0 ,0, 255) Nothing
multiline3 = shape $ MultiLine [Point (350, 350), Point (375, 300), Point (400, 450)]
circ2 = Circle (Point (350, 350)) 1 (0, 255, 0, 255) 0 (0,0,0,0) Nothing
circ3 = Circle (Point (375, 300)) 1 (0, 255, 0, 255) 0 (0,0,0,0) Nothing
circ4 = Circle (Point (400, 450)) 1 (0, 255, 0, 255) 0 (0,0,0,0) Nothing
polygon3 = Polygon [Point (465, 465), Point (465, 490), Point (460, 490), Point (470, 502), Point (480, 490), Point (475, 490), Point (475, 465)] (0, 255, 0, 125) 10 (255, 0, 0, 255) Nothing
polygon4 = Polygon [Point (465, 465), Point (465, 490), Point (460, 490), Point (470, 502), Point (480, 490), Point (475, 490), Point (475, 465)] (0, 255, 0, 0) 2 (0, 0, 0, 255) Nothing


middlePoint :: Shape
middlePoint
    = shape $ Circle (Point (0.5 * fromIntegral width, 0.5 * fromIntegral height)) 4 (125, 125, 125, 200)
    where
        (width, height) = canvasDimensions


rotatePoint :: (ToCenter a) => a -> Shape
rotatePoint shape_
    = Circle center 4 (255, 255, 0, 255) 1 (0, 0, 0, 255) Nothing
    where
        center = toCenter shape_


bbox :: Shape -> Shape
bbox shape
    = bboxShape shape


bboxShape :: (ToBoundingBox a) => a -> Shape
bboxShape a
    =  MultiLine [ll, ul, ur, lr, ll] 1 (0, 0, 0, 255) Nothing
    where
        bb@(BoundingBox ll ul ur lr) = toBoundingBox a


eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start = (ps, [ OutCanvas $ C.SetupCanvas 1 1 canvasDimensions (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                          , OutBasicShapes $ DrawShapes 1 shapes
                          --, OutCanvas $ TeardownCanvas 1
                          , Stop
                          ])
                  where
                    shapes = [ rect
                             , bbox rect
                             , rotatePoint rect
                              --{-
                             , circ
                             , bbox circ
                             , rotatePoint circ

                             , text_
                             , bbox text_
                             , rotatePoint text_

                             , polygon1
                             , bbox polygon1
                             , rotatePoint polygon1

                             , polygon2
                             , bbox polygon2
                             , rotatePoint polygon2

                             , line
                             , bbox line
                             , rotatePoint line

                             , multiline
                             , bbox multiline
                             , rotatePoint multiline
                             , multiline2
                                  
                             , multiline3
                             , circ2
                     , circ3
                             , circ4
                             , polygon3
                             , bbox polygon3
                             , rotatePoint polygon3
                             , polygon4

                             , rotatePoint boundBox
                             , bboxShape $ rotateLeftAround bboxRotatePoint 45 boundBox
                             ---}
                             , middlePoint
                             ]
                    boundBox = (BoundingBox (Point (0,400)) (Point (0, 450)) (Point (100, 450)) (Point (100, 400)))
                    bboxRotatePoint = toCenter boundBox
                          
                          
start = startEventloopSystem eventloopConfiguration
