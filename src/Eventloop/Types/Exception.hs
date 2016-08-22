{-# LANGUAGE DeriveDataTypeable #-}

module Eventloop.Types.Exception where


import Control.Exception
import Data.Typeable

import Eventloop.Types.Common
import Eventloop.Types.Events


data EventloopException = ShuttingDownException
                        | RequestShutdownException
                        | NoOutRouteException Out
                        | InitializationException EventloopModuleIdentifier SomeException
                        | RetrievingException EventloopModuleIdentifier SomeException
                        | ProcessingException ProcessingDescription EventloopModuleIdentifier SomeException
                        | EventloopException SomeException
                        | SendingException EventloopModuleIdentifier Out SomeException
                        | TeardownException EventloopModuleIdentifier SomeException
                        
                        deriving (Typeable)

instance Exception EventloopException


instance Show EventloopException where
    show ShuttingDownException    = exceptionMessage "A shutting down exception has been logged. This should not happen. Please contact an administrator."
                                                     []
    show RequestShutdownException = "System is shutting down..."
    
    show (NoOutRouteException out)  = exceptionMessage "Tried to route an Out even to a module but could not find an appropriate configured event sender."
                                                       [ ("Out event", show out)
                                                       ]
    show (InitializationException moduleId e) = exceptionMessage "Tried to initialize a module but something happened."
                                                                 [ ("Module", moduleId)
                                                                 , ("Exception", show e)
                                                                 ]
    show (RetrievingException moduleId e) = exceptionMessage "Tried to retrieve an In event from a module but something happened. The retriever has been shutdown."
                                                             [ ("Module", moduleId)
                                                             , ("Exception", show e)
                                                             ]
    show (ProcessingException processDesc moduleId e) = exceptionMessage "Tried to process an In\\Out event with a module but something happened."
                                                                         [ ("Processor", processDesc)
                                                                         , ("Module", moduleId)
                                                                         , ("Exception", show e)
                                                                         ]
    show (EventloopException e) = exceptionMessage "An exception occurred in your eventloop program."
                                                   [ ("Exception", show e)
                                                   ]
    show (SendingException moduleId out e) = exceptionMessage "Tried to send an Out event with a module but something happened."
                                                              [ ("Module", moduleId)
                                                              , ("Out event", show out)
                                                              , ("Exception", show e)
                                                              ]
    show (TeardownException moduleId e) = exceptionMessage "Tried to teardown a module but something happened."
                                                           [ ("Module", moduleId)
                                                           , ("Exception", show e)
                                                           ]

exceptionMessage :: String
                 -> [(String, String)]
                 -> String
exceptionMessage description fields
    = "- Exc: " ++ description ++ "\n" ++ (concat fieldLines)
    where
        fieldLines = map (\(field, value) -> "    " ++ field ++ ": " ++ value ++ "\n") fields