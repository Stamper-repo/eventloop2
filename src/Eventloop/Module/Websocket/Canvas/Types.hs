module Eventloop.Module.Websocket.Canvas.Types where

import Control.Concurrent.MVar

import Eventloop.Types.Common

type CanvasSystemReceiveBuffer = MVar SystemCanvasIn

instance Show (MVar a) where
    show _ = "CanvasSystemReceiveBuffer"


type Opcode = Int

type ScreenMetric = Int
type ScreenX = ScreenMetric
type ScreenY = ScreenMetric
type ScreenWidth = ScreenMetric
type ScreenHeight = ScreenMetric
type ScreenRadius = ScreenMetric
type ScreenAngle = ScreenMetric -- ^In degrees

type ScreenPoint = (ScreenX, ScreenY)
type ScreenDimensions = (ScreenWidth, ScreenHeight)

type ScreenStartingPoint = ScreenPoint
type ScreenControlPoint = ScreenPoint
type ScreenEndPoint = ScreenPoint

type ScreenStartingAngle = ScreenAngle
type ScreenEndAngle = ScreenAngle

type CanvasId = NumericId
type ZIndex = Int

type ScreenColorMetric = Int
type ScreenRed = ScreenColorMetric
type ScreenGreen = ScreenColorMetric
type ScreenBlue = ScreenColorMetric
type ScreenAlpha = Float
type ScreenColor = (ScreenRed, ScreenGreen, ScreenBlue, ScreenAlpha)
type ScreenPixel = ScreenColor

type ColorStopOffset = Float

type ScreenCircle = (ScreenPoint, ScreenRadius)

type ScaleUnit = Float
type ScaleX = ScaleUnit
type ScaleY = ScaleUnit

type ScreenLineThickness = ScreenMetric

type FontFamily = [Char]
type FontSize = Int -- In Pixels


data RoutedMessageIn = InUserCanvas CanvasIn
                     | InSystemCanvas SystemCanvasIn
                     deriving (Eq, Show)
                      
data RoutedMessageOut = OutUserCanvas CanvasOut
                      | OutSystemCanvas SystemCanvasOut
                      deriving (Eq, Show)

{- |Opcode: 2100-}
data SystemCanvasIn = SystemMeasuredText CanvasText ScreenDimensions {- ^Opcode: 2101-}
                    deriving (Eq, Show)
                      
{- |Opcode: 2000-}
data SystemCanvasOut = SystemMeasureText CanvasText  {- ^Opcode: 2001-}
                     deriving (Eq, Show)
                      
{- |Opcode: 0100-}
data CanvasIn = MeasuredText CanvasText ScreenDimensions {- ^Opcode: 0101-}
              deriving (Eq, Show)


{- Main types -}
{- | Reserved type words
     Type: t      | Opcode
     Arguments: a | List of arguments for that data type
     Route: r     | Either 's' for system or 'u' for user
     Object: o    | The object that is beneath
     
     Example:
     {'r': 's', 'o': {SystemMeasuredText object}}
     
     SystemMeasuredText object: {'t':2102, 'a':[CanvasId, CanvasText object, ScreenDimensions]}
-}

{- |Opcode: 0200-}
data CanvasOut = SetupCanvas CanvasId ZIndex ScreenDimensions CSSPosition {- ^Opcode: 0201-}
               | TeardownCanvas CanvasId {- ^Opcode: 0202-}
               | CanvasOperations CanvasId [CanvasOperation] {- ^Opcode: 0203-}
               | MeasureText CanvasText {- ^Opcode: 0204-}
               deriving (Eq, Show)
               
{- |Opcode: 0300-}
data CanvasOperation = DrawPath ScreenStartingPoint [ScreenPathPart] PathStroke PathFill {- ^Opcode: 0301 -}
                     | DrawText CanvasText ScreenPoint TextStroke TextFill {- ^Opcode: 0302 -}
                     | DoTransform CanvasTransform {- ^Opcode: 0303 -}
                     | Clear ClearPart {- ^Opcode: 0304 -}
                     | Frame {- ^Opcode: 0305 -}
                     deriving (Eq, Show)

{- |Opcode: 0400-}
data ScreenPathPart = MoveTo ScreenPoint {- ^Opcode: 0401-}
                    | LineTo ScreenPoint {- ^Opcode: 0402-}
                    | BezierCurveTo ScreenControlPoint ScreenControlPoint ScreenEndPoint {- ^Opcode: 0403-}
                    | QuadraticCurveTo ScreenControlPoint ScreenEndPoint {- ^Opcode: 0404-}
                    | ArcTo ScreenControlPoint ScreenControlPoint ScreenRadius {- ^Opcode: 0405-}
                    | Arc ScreenCircle ScreenStartingAngle ScreenEndAngle {- ^Opcode: 0406-}
                    | Rectangle ScreenPoint ScreenDimensions {- ^Opcode: 0407-}
                    deriving (Eq, Show)

{- Styling of Shapes -}
{- |Opcode: 0500-}
type PathRenderStrokeStyle = RenderStyle

data PathStroke = PathStroke ScreenLineThickness PathRenderStrokeStyle {- ^Opcode: 0501-}
                | NoPathStroke {- ^Opcode: 0502-}
                deriving (Eq, Show)

{- |Opcode: 0600-}
type PathRenderFillStyle = RenderStyle

data PathFill = PathFill PathRenderFillStyle {- ^Opcode: 0601-}
              | NoPathFill {- ^Opcode: 0602-}
              deriving (Eq, Show)

{- |Opcode: 0700-}
type CanvasColorStop = (ColorStopOffset, ScreenColor)

data RenderStyle = CanvasColor ScreenColor {- ^Opcode: 0701-}
                 | CanvasGradient CanvasGradientType [CanvasColorStop] {- ^Opcode:0702-}
                 | CanvasPattern CanvasImage PatternRepetition {- ^Opcode: 0703-}
                     deriving (Eq, Show)

{- |Opcode: 0800-}                     
data CanvasImage = CanvasElement CanvasId ScreenPoint ScreenDimensions {- ^Opcode: 0801-}
                 | ImageData ScreenDimensions [ScreenPixel] {- ^Opcode: 0802 
                                                            [ScreenPixel] should be as long as width * height * 4
                                                            -}
                 deriving (Eq, Show)
{- |Opcode: 0900-}                 
data PatternRepetition = Repeat {- ^Opcode: 0901-}
                       | RepeatX {- ^Opcode: 0902-}
                       | RepeatY {- ^Opcode: 0903-} 
                       | NoRepeat {- ^Opcode: 0904-}
                       deriving (Eq, Show)

{- |Opcode: 1000-}
data CanvasGradientType = RadialGradient ScreenCircle ScreenCircle {- ^Opcode: 1001
                                                                       First circle = inner circle, Second circle is enclosing circle
                                                                   -}
                        | LinearGradient ScreenPoint ScreenPoint {- ^Opcode: 1002-}
                        deriving (Eq, Show)

{- To Draw Text -}
{- |Opcode: 1200-}
data CanvasText = CanvasText [Char] Font Alignment {- ^Opcode: 1201-}
                deriving (Eq, Show)

{- |Opcode: 1300-}
data Font = Font FontFamily FontSize {- ^Opcode: 1301-}
          deriving (Eq, Show)

{- |Opcode: 1400-}
type TextStrokeRenderStyle = RenderStyle
type TextFillRenderStyle = RenderStyle

data TextStroke = TextStroke ScreenLineThickness TextStrokeRenderStyle {- ^Opcode: 1401-}
                | NoTextStroke {- ^Opcode: 1402-}
                deriving (Eq, Show)

{- |Opcode: 2400-}
data TextFill = TextFill TextFillRenderStyle {- ^Opcode: 2401-}
              | NoTextFill {- ^Opcode: 2402-}
              deriving (Eq, Show)

{- |Opcode: 1500-}
data Alignment = AlignLeft {- ^Opcode: 1501-}
               | AlignRight {- ^Opcode: 1502-}
               | AlignCenter {- ^Opcode: 1503-}
               | AlignStart {- ^Opcode: 1504-}
               | AlignEnd {- ^Opcode: 1505-}
               deriving (Eq, Show)
               
{- Transform The Canvas -}
{- |Opcode: 1600-}
type TransformUnit = Float                        
type TransformationMatrix  = (TransformUnit, TransformUnit, TransformUnit, TransformUnit, TransformUnit, TransformUnit)

data CanvasTransform = Save {- ^Opcode: 1601-}
                     | Restore {- ^Opcode: 1602-}
                     | Translate ScreenPoint {- ^Opcode: 1603-}
                     | Rotate ScreenAngle {- ^Opcode: 1604-}
                     | Scale ScaleX ScaleY {- ^Opcode: 1605-}
                     | Transform TransformationMatrix {- ^Opcode: 1606-}
                     | SetTransform TransformationMatrix {- ^Opcode: 1607-}
                     | ResetTransform {- ^Opcode: 1608-}
                     deriving (Eq, Show)

{- CSS Position of DOM elements -}
{- |Opcode: 2200-}
type CSSLeftOffset = CSSUnit
type CSSTopOffset = CSSUnit
type CSSMeasurements = (CSSLeftOffset, CSSTopOffset)

data CSSPosition = CSSPosition CSSBindPoint CSSMeasurements {- ^Opcode: 2201-}
                 deriving (Eq, Show)

{- |Opcode: 2300-}
data CSSBindPoint = CSSFromCenter {- ^Opcode: 2301-}
                  | CSSFromDefault {- ^Opcode: 2302 Usually this is the top left corner of the element -}
                  deriving (Eq, Show)
                     
{- |Opcode: 1800-}
data CSSUnit = CSSPixels Int {- ^Opcode: 1801-}
             | CSSPercentage Int {- ^Opcode: 1802-}
             deriving (Eq, Show)
             
{- |Opcode: 1900 -}
data ClearPart = ClearRectangle ScreenPoint ScreenDimensions {- ^Opcode: 1901-}
               | ClearCanvas {- ^Opcode: 1902-}
               deriving (Eq, Show)