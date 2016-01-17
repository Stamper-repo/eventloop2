module Eventloop.Module.BasicShapes.MeasureTextHack where

import Data.IORef
import System.IO.Unsafe

import Eventloop.Module.Websocket.Canvas.Types


measureTextRef :: (IORef (CanvasText -> IO ScreenDimensions))
{-# NOINLINE measureTextRef #-}
measureTextRef = unsafePerformIO (newIORef undefined)


saveMeasureText :: (CanvasText -> IO ScreenDimensions) ->
                IO ()
saveMeasureText f
    = writeIORef measureTextRef f


useMeasureText :: CanvasText -> ScreenDimensions
useMeasureText text
    = unsafePerformIO $ do
        measureText <- readIORef measureTextRef
        measureText text
