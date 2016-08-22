module Eventloop.Module.Websocket.Canvas.Opcode where

import Eventloop.Module.Websocket.Canvas.Types

class ToOpcode a where
    toOpcode :: a -> Opcode

    
instance ToOpcode SystemCanvasOut where
    toOpcode (SystemMeasureText _) = 2001

instance ToOpcode CanvasOut where
    toOpcode (SetupCanvas _ _ _ _)  = 201
    toOpcode (TeardownCanvas _)     = 202
    toOpcode (CanvasOperations _ _) = 203
    toOpcode (MeasureText _)        = 204

instance ToOpcode CanvasOperation where
    toOpcode (DrawPath _ _ _ _) = 301
    toOpcode (DrawText _ _ _ _) = 302
    toOpcode (DoTransform _)    = 303
    toOpcode (Clear _)          = 304
    toOpcode (Frame)            = 305

instance ToOpcode ScreenPathPart where
    toOpcode (MoveTo _)              = 401
    toOpcode (LineTo _)              = 402
    toOpcode (BezierCurveTo _ _ _)   = 403
    toOpcode (QuadraticCurveTo _ _)  = 404
    toOpcode (ArcTo _ _ _)           = 405
    toOpcode (Arc _ _ _)             = 406
    toOpcode (Rectangle _ _)         = 407
    toOpcode (ClosePath)             = 408

instance ToOpcode PathStroke where
    toOpcode (PathStroke _ _) = 501
    toOpcode (NoPathStroke) = 502

instance ToOpcode PathFill where
    toOpcode (PathFill _) = 601
    toOpcode (NoPathFill) = 602

instance ToOpcode RenderStyle where
    toOpcode (CanvasColor _)      = 701
    toOpcode (CanvasGradient _ _) = 702
    toOpcode (CanvasPattern _ _)  = 703

instance ToOpcode CanvasImage where
    toOpcode (CanvasElement _ _ _) = 801
    toOpcode (ImageData _ _)       = 802

instance ToOpcode PatternRepetition where
    toOpcode (Repeat)   = 901
    toOpcode (RepeatX)  = 902
    toOpcode (RepeatY)  = 903
    toOpcode (NoRepeat) = 0904
    
instance ToOpcode CanvasGradientType where
    toOpcode (RadialGradient _ _) = 1001
    toOpcode (LinearGradient _ _) = 1002
    
instance ToOpcode CanvasText where
    toOpcode (CanvasText _ _ _) = 1201
    
instance ToOpcode Font where
    toOpcode (Font _ _) = 1301
    
instance ToOpcode TextStroke where
    toOpcode (TextStroke _ _) = 1401
    toOpcode (NoTextStroke)   = 1402
    
instance ToOpcode TextFill where
    toOpcode (TextFill _) = 2401
    toOpcode (NoTextFill) = 2402
    
instance ToOpcode Alignment where
    toOpcode (AlignLeft)   = 1501
    toOpcode (AlignRight)  = 1502
    toOpcode (AlignCenter) = 1503
    
instance ToOpcode CanvasTransform where
    toOpcode (Save)           = 1601
    toOpcode (Restore)        = 1602
    toOpcode (Translate _)    = 1603
    toOpcode (Rotate _)       = 1604
    toOpcode (Scale _ _)      = 1605
    toOpcode (Transform _)    = 1606
    toOpcode (SetTransform _) = 1607
    toOpcode (ResetTransform) = 1608
    
instance ToOpcode CSSUnit where
    toOpcode (CSSPixels _)     = 1801
    toOpcode (CSSPercentage _) = 1802
    
instance ToOpcode ClearPart where
    toOpcode (ClearRectangle _ _) = 1901
    toOpcode (ClearCanvas)        = 1902
    
instance ToOpcode CSSPosition where
    toOpcode (CSSPosition _ _) = 2201
    
instance ToOpcode CSSBindPoint where
    toOpcode (CSSFromCenter)  = 2301
    toOpcode (CSSFromDefault) = 2302 