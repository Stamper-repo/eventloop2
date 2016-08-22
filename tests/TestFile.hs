module TestFile where

import Prelude

import Eventloop.Core
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.DefaultConfiguration
import Eventloop.Module.File
import Eventloop.Module.StdOut
import System.IO

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration = defaultConfig { setupModuleConfigurations = [ setupFileModuleConfiguration
                                                                     , setupStdOutModuleConfiguration
                                                                     ]
                                }
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop


filepath = "TestFile.txt"
lineToWrite = "hello!\n"

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop state Start = (state, [OutFile (OpenFile filepath ReadWriteMode)])

eventloop state (InFile (FileOpened path succes)) | succes    = (state, [OutFile $ RetrieveChar filepath])
                                                  | otherwise = (state, [])

eventloop state (InFile (RetrievedChar fp char)) = (state, [OutFile $ RetrieveLine fp, OutStdOut $ StdOutMessage outStr])
                                                where
                                                    outStr = "First Char: " ++ [char] ++ "\n"
                                                  
eventloop state (InFile (RetrievedLine fp line)) = (state, [OutFile $ RetrieveContents fp, OutStdOut (StdOutMessage outStr)])
                                                where
                                                    outStr = "First line: " ++ line ++ "\n"
                                                  
eventloop state (InFile (RetrievedContents fp lines)) = (state, [OutFile $ IfEOF filepath, OutStdOut (StdOutMessage outStr)])
                                                        where
                                                            outStr = "Rest of file: " ++ concat lines ++ "\n"
                                                            
eventloop state (InFile (IsEOF fp isEOF)) = (state, [OutFile $ WriteTo fp lineToWrite, OutStdOut $ StdOutMessage outStr])
                                           where
                                            outStr = "File is at EOF: " ++ show isEOF ++ "\n"
             
eventloop state (InFile (WroteTo fp success)) = (state, [OutFile $ CloseFile fp, OutStdOut $ StdOutMessage outStr])
                                                where
                                                    outStr = "Wrote '" ++ lineToWrite ++ "' to file\n"
             
eventloop state (InFile (FileClosed path success)) = (state, [OutStdOut (StdOutMessage ("File is now closed: " ++ show success ++ ['\n'])), Stop])

start = startEventloopSystem eventloopConfiguration