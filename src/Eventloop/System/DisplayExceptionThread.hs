module Eventloop.System.DisplayExceptionThread
    ( startDisplayingExceptions
    ) where

import qualified Control.Exception as E
import Control.Concurrent.ExceptionCollection
import Control.Concurrent.SafePrint

import Eventloop.Types.Exception
import Eventloop.Types.System


startDisplayingExceptions :: EventloopSystemConfiguration progstateT
                          -> IO ()
startDisplayingExceptions systemConfig
    = do
        exceptions_ <- collectExceptions (exceptions systemConfig)
        mapM_ (displayException safePrintToken_) exceptions_
    where
        safePrintToken_ = safePrintToken (sharedIOConstants systemConfig)
          
          
displayException :: SafePrintToken
                 -> E.SomeException
                 -> IO ()
displayException safePrintToken exception = safePrintLn safePrintToken (show exception)