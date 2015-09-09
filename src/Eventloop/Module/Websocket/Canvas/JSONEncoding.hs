{-# LANGUAGE OverloadedStrings #-}
module Eventloop.Module.Websocket.Canvas.JSONEncoding where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad

import Eventloop.Module.Websocket.Canvas.Types
import Eventloop.Module.Websocket.Canvas.Opcode


instance FromJSON RoutedMessageIn where
    parseJSON (Object v) = do
                            route <- v .: "r" :: Parser [Char]
                            obj <- v .: "o"
                            case route of
                                        "s" -> InSystemCanvas <$> (parseJSON obj)
                                        "u" -> InUserCanvas <$> (parseJSON obj)

instance FromJSON SystemCanvasIn where
    parseJSON (Object v) = do
                            opCode <- v .: "t" :: Parser Int
                            case opCode of
                                        2101 -> SystemMeasuredText <$> v .: "canvasid" 
                                                                <*> (v .: "canvastext" >>= parseJSON)
                                                                <*> ( (\width height -> (width, height)) 
                                                                        <$> v .: "width"
                                                                        <*> v .: "height"
                                                                    )

instance FromJSON CanvasIn where
    parseJSON (Object v) = do
                            opCode <- v .: "t" :: Parser Int
                            case opCode of
                                        101 -> MeasuredText <$> v .: "canvasid" 
                                                            <*> (v .: "canvastext" >>= parseJSON)
                                                            <*> ( (\width height -> (width, height)) 
                                                                <$> v .: "width"
                                                                <*> v .: "height"
                                                                )

instance FromJSON CanvasText where
    parseJSON (Object v) = CanvasText <$>  v .: "text" <*> (v .: "font" >>= parseJSON) <*> (v .: "alignment" >>= parseJSON)

    
instance FromJSON Font where
    parseJSON (Object v) = Font <$> v .: "fontfamily" <*> v .: "fontsize"

    
instance FromJSON Alignment where
    parseJSON (Object v) = do
                            alignment <- v .: "t" :: Parser Int
                            return $ case alignment of
                                        1501 -> AlignLeft
                                        1502 -> AlignRight
                                        1503 -> AlignCenter
                                        1504 -> AlignStart
                                        1505 -> AlignEnd
                            
    
    
operationObject :: Opcode -> [Value] -> Value
operationObject opcode []               = object ["t" .= opcode]
operationObject opcode encodedArguments = object ["t" .= opcode, "a" .= encodedArguments]


instance ToJSON RoutedMessageOut where
    toJSON d@(OutUserCanvas canvasOut)   = object ["r" .= route, "o" .= canvasOut]
                                        where
                                            route = "u" :: [Char]
    toJSON d@(OutSystemCanvas canvasOut) = object ["r" .= route, "o" .= canvasOut]
                                        where
                                            route = "s" :: [Char]


instance ToJSON SystemCanvasOut where
    toJSON d@(SystemMeasureText canvasId canvasText) = operationObject (toOpcode d) [ toJSON canvasId
                                                                                    , toJSON canvasText
                                                                                    ]


instance ToJSON CanvasOut where
    toJSON d@(SetupCanvas canvasId zIndex screenDimensions cssPosition) = operationObject (toOpcode d) [ toJSON canvasId
                                                                                                       , toJSON zIndex
                                                                                                       , toJSON screenDimensions
                                                                                                       , toJSON cssPosition
                                                                                                       ]
    toJSON d@(TeardownCanvas canvasId) = operationObject (toOpcode d) [toJSON canvasId]
    toJSON d@(CanvasOperations canvasId canvasOperations) = operationObject (toOpcode d) [ toJSON canvasId
                                                                                         , toJSON canvasOperations
                                                                                         ]
    toJSON d@(MeasureText canvasId text) = operationObject (toOpcode d) [ toJSON canvasId
                                                                        , toJSON text
                                                                        ]

    
instance ToJSON CanvasOperation where
    toJSON d@(DrawPath screenStartingPoint screenPathParts pathStroke pathFill) = operationObject (toOpcode d) [ toJSON screenStartingPoint
                                                                                                               , toJSON screenPathParts
                                                                                                               , toJSON pathStroke
                                                                                                               , toJSON pathFill
                                                                                                               ]
    toJSON d@(DrawText canvasText screenPoint textStroke textFill) = operationObject (toOpcode d) [ toJSON canvasText
                                                                                                  , toJSON screenPoint
                                                                                                  , toJSON textStroke
                                                                                                  , toJSON textFill
                                                                                                  ]
    toJSON d@(DoTransform canvasTransform) = operationObject (toOpcode d) [toJSON canvasTransform]
    toJSON d@(Clear clearPart) = operationObject (toOpcode d) [toJSON clearPart]

    
instance ToJSON ScreenPathPart where
    toJSON d@(MoveTo screenPoint) = operationObject (toOpcode d) [toJSON screenPoint]
    toJSON d@(LineTo screenPoint) = operationObject (toOpcode d) [toJSON screenPoint]
    toJSON d@(BezierCurveTo screenControlPoint1 screenControlPoint2 screenEndPoint) = operationObject (toOpcode d) [ toJSON screenControlPoint1
                                                                                                                   , toJSON screenControlPoint2
                                                                                                                   , toJSON screenEndPoint
                                                                                                                   ]
    toJSON d@(QuadraticCurveTo screenControlPoint screenEndPoint) = operationObject (toOpcode d) [ toJSON screenControlPoint
                                                                                                  , toJSON screenEndPoint
                                                                                                  ]
    toJSON d@(ArcTo screenControlPoint1 screenControlPoint2 screenRadius) = operationObject (toOpcode d) [ toJSON screenControlPoint1
                                                                                                         , toJSON screenControlPoint2
                                                                                                         , toJSON screenRadius
                                                                                                         ]
    toJSON d@(Arc screenCircle screenStartingAngle screenEndAngle) = operationObject (toOpcode d) [ toJSON screenCircle
                                                                                                  , toJSON screenStartingAngle
                                                                                                  , toJSON screenEndAngle
                                                                                                  ]
    toJSON d@(Rectangle screenPoint screenDimensions) = operationObject (toOpcode d) [ toJSON screenPoint
                                                                                     , toJSON screenDimensions
                                                                                     ]

    
instance ToJSON PathStroke where
    toJSON d@(PathStroke screenLineThickness pathRenderStrokeStyle) = operationObject (toOpcode d) [ toJSON screenLineThickness
                                                                                                   , toJSON pathRenderStrokeStyle
                                                                                                   ]
    toJSON d = operationObject (toOpcode d) []

    
instance ToJSON PathFill where
    toJSON d@(PathFill pathRenderFillStyle) = operationObject (toOpcode d) [toJSON pathRenderFillStyle]
    toJSON d = operationObject (toOpcode d) []
    

instance ToJSON RenderStyle where
    toJSON d@(CanvasColor screenColor) = operationObject (toOpcode d) [toJSON screenColor]
    toJSON d@(CanvasGradient canvasGradientType canvasColorStops) = operationObject (toOpcode d) [ toJSON canvasGradientType
                                                                                                 , toJSON canvasColorStops
                                                                                                 ]
                                                                                                 
    toJSON d@(CanvasPattern canvasImage patternRepetition) = operationObject (toOpcode d) [ toJSON canvasImage
                                                                                          , toJSON patternRepetition
                                                                                          ]

    
instance ToJSON CanvasImage where
    toJSON d@(CanvasElement canvasId screenPoint screenDimensions) = operationObject (toOpcode d) [ toJSON canvasId
                                                                                                  , toJSON screenPoint
                                                                                                  , toJSON screenDimensions
                                                                                                  ]
                                                                                                  
    toJSON d@(ImageData screenDimensions screenPixels) = operationObject (toOpcode d) [ toJSON screenDimensions
                                                                                      , toJSON screenPixels
                                                                                      ]

instance ToJSON PatternRepetition where
    toJSON d = operationObject (toOpcode d) []


instance ToJSON CanvasGradientType where
    toJSON d@(RadialGradient screenCircle1 screenCircle2) = operationObject (toOpcode d) [ toJSON screenCircle1
                                                                                         , toJSON screenCircle2 
                                                                                         ]
                                                                                         
    toJSON d@(LinearGradient screenPoint1 screenPoint2) = operationObject (toOpcode d) [ toJSON screenPoint1
                                                                                       , toJSON screenPoint2 
                                                                                       ]


instance ToJSON CanvasText where
    toJSON d@(CanvasText text font alignment) = operationObject (toOpcode d) [ toJSON text
                                                                             , toJSON font
                                                                             , toJSON alignment
                                                                             ]


instance ToJSON Font where
    toJSON d@(Font fontFamily fontSize) = operationObject (toOpcode d) [ toJSON fontFamily
                                                                       , toJSON fontSize
                                                                       ]

    
instance ToJSON TextStroke where
    toJSON d@(TextStroke screenLineThickness textRenderStyle) = operationObject (toOpcode d) [ toJSON screenLineThickness
                                                                                             , toJSON textRenderStyle
                                                                                             ]
    toJSON d@(NoTextStroke) = operationObject (toOpcode d) []

instance ToJSON TextFill where
    toJSON d@(TextFill textRenderStyle) = operationObject (toOpcode d) [ toJSON textRenderStyle
                                                                       ]
    toJSON d@(NoTextFill) = operationObject (toOpcode d) []
    
    
instance ToJSON Alignment where
    toJSON d = operationObject (toOpcode d) []

    
instance ToJSON CanvasTransform where
    toJSON d@(Translate screenPoint) = operationObject (toOpcode d) [toJSON screenPoint]
    toJSON d@(Rotate screenAngle)    = operationObject (toOpcode d) [toJSON screenAngle]
    toJSON d@(Scale x y)             = operationObject (toOpcode d) [toJSON x, toJSON y]
    toJSON d@(Transform tm)          = operationObject (toOpcode d) [toJSON tm]
    toJSON d@(SetTransform tm)       = operationObject (toOpcode d) [toJSON tm]
    toJSON d                         = operationObject (toOpcode d) []
    
    
instance ToJSON CSSPosition where
    toJSON d@(CSSPosition cssBindPoint cssMeasurements) = operationObject (toOpcode d) [ toJSON cssBindPoint
                                                                                       , toJSON cssMeasurements
                                                                                       ]

instance ToJSON CSSBindPoint where
    toJSON d = operationObject (toOpcode d) []
                                                                                      
instance ToJSON CSSUnit where
    toJSON d@(CSSPixels i)     = operationObject (toOpcode d) [toJSON i]
    toJSON d@(CSSPercentage i) = operationObject (toOpcode d) [toJSON i]
    
    
instance ToJSON ClearPart where
    toJSON d@(ClearRectangle screenPoint screenDimensions) = operationObject (toOpcode d) [ toJSON screenPoint
                                                                                          , toJSON screenDimensions
                                                                                          ]
    toJSON d = operationObject (toOpcode d) []