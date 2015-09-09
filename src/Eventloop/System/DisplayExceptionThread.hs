module Eventloop.System.DisplayExceptionThread
    ( startDisplayingExceptions
    ) where

import Control.Concurrent.ExceptionCollection

import Eventloop.Types.Exception
import Eventloop.Types.System


startDisplayingExceptions :: EventloopSystemConfiguration progstateT
                          -> IO ()
startDisplayingExceptions systemConfig
    = do
        exceptions_ <- collectExceptions (exceptions systemConfig)
        mapM_ displayException exceptions_
          
          
displayException :: EventloopException
                 -> IO ()
displayException eventloopException = putStrLn (show eventloopException)