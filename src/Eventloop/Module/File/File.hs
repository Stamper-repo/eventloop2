module Eventloop.Module.File.File
    ( setupFileModuleConfiguration
    , fileModuleIdentifier
    , fileEventRetriever
    , fileEventSender
    , fileTeardown
    ) where

import Data.Maybe
import System.IO

import Eventloop.Module.File.Types
import Eventloop.Types.Common
import Eventloop.Types.Events
import Eventloop.Types.System

setupFileModuleConfiguration :: EventloopSetupModuleConfiguration
setupFileModuleConfiguration = ( EventloopSetupModuleConfiguration 
                                    fileModuleIdentifier
                                    (Just fileInitializer)
                                    (Just fileEventRetriever)
                                    Nothing
                                    Nothing
                                    (Just fileEventSender)
                                    (Just fileTeardown)
                                 )

fileModuleIdentifier :: EventloopModuleIdentifier
fileModuleIdentifier = "file"


fileInitializer :: Initializer
fileInitializer sharedIO
    = return (sharedIO, FileState [] [])

    
fileEventRetriever :: EventRetriever
fileEventRetriever sharedIO fs
    = return (sharedIO, fs', newFileInEvents')
    where
        newFileInEvents' = map InFile (newFileInEvents fs)
        fs' = fs {newFileInEvents = []}


fileEventSender :: EventSender
fileEventSender sharedIO fileState (OutFile a)
    = do
        fileState' <- fileEventSender' fileState a                                                
        return (sharedIO, fileState')

    
fileEventSender' :: IOState -> FileOut -> IO IOState
fileEventSender' fs (OpenFile filepath iomode) = do
                                                    handle <- openFile filepath iomode
                                                    let
                                                        fileOpenedEvent = FileOpened filepath True
                                                        opened' = (opened fs) ++ [(filepath, handle, iomode)]
                                                        otherInEvents = newFileInEvents fs
                                                        fs' = fs {newFileInEvents=(otherInEvents ++ [fileOpenedEvent]), opened=opened'}
                                                    return fs'
                                                                                         
fileEventSender' fs (CloseFile filepath) | openfileM == Nothing = return fs
                                         | otherwise = do
                                                        hClose handle
                                                        return fs'
                                         where
                                             openedFiles = opened fs
                                             openfileM = retrieveOpenedFile openedFiles filepath
                                             (fp, handle, iomode) = fromJust openfileM
                                             openedFiles' = removeOpenedFile openedFiles filepath
                                             closedFileEvent = FileClosed filepath True
                                             fs' = fs {newFileInEvents=(newFileInEvents fs ++ [closedFileEvent]), opened=openedFiles'}
                                                        
fileEventSender' fs (RetrieveContents filepath) = doReadAction filepath fs RetrievedContents retrieveContents
fileEventSender' fs (RetrieveLine filepath)     = doReadAction filepath fs RetrievedLine hGetLine
fileEventSender' fs (RetrieveChar filepath)     = doReadAction filepath fs RetrievedChar hGetChar                           
fileEventSender' fs (IfEOF filepath)            = getFromFile filepath fs fileIsOpened IsEOF hIsEOF
                                                                     
fileEventSender' fs (WriteTo filepath contents) | fileIsWriteable (opened fs) filepath = do
                                                                                            hPutStr handle contents
                                                                                            let
                                                                                                fs' = fs {newFileInEvents = (newFileInEvents fs) ++ [WroteTo filepath True]}
                                                                                            return fs'
                                                | otherwise = return fs
                                                where
                                                    Just (fp, handle, iomode) = retrieveOpenedFile (opened fs) filepath

                                                                                
doReadAction :: FilePath -> IOState -> (FilePath -> a -> FileIn) -> (Handle -> IO a) -> IO IOState
doReadAction filepath fs inEvent readAction = getFromFile filepath fs fileIsReadable inEvent readAction
                         
                         
getFromFile :: FilePath -> 
               IOState -> 
               ([OpenFile] -> FilePath -> Bool) -> {- Check if the action should be done -}
               (FilePath -> a -> FileIn) -> {- The inEvent Constructor -}
               (Handle -> IO a) ->  {- The action which will grant a result -}
               IO IOState
getFromFile filepath fs@(FileState { opened = opened
                                    , newFileInEvents = newFileInEvents
                                    }) fileCheck inEvent action | fileCheck opened filepath = do
                                                                                                    result <- action handle
                                                                                                    let
                                                                                                        newInEvent = inEvent filepath result
                                                                                                        fs' = fs {newFileInEvents=newFileInEvents ++ [newInEvent]}
                                                                                                    return fs'
                                                                | otherwise                 = return fs
                                                                 where
                                                                    Just (fp, handle, iomode) = retrieveOpenedFile opened filepath
                                                            

fileIsReadable :: [OpenFile] -> FilePath -> Bool
fileIsReadable opened filepath | fileIsOpened opened filepath = iomode == ReadMode || iomode == ReadWriteMode
                               | otherwise                    = False
                              where
                                  Just (fp, handle, iomode) = retrieveOpenedFile opened filepath

                                  
fileIsWriteable :: [OpenFile] -> FilePath -> Bool
fileIsWriteable opened filepath | fileIsOpened opened filepath = iomode == WriteMode || iomode == ReadWriteMode || iomode == AppendMode
                                | otherwise                    = False
                                where
                                    Just (fp, handle, iomode) = retrieveOpenedFile opened filepath

                                  
fileIsOpened :: [OpenFile] -> FilePath -> Bool
fileIsOpened opened filepath = not (openedFileM == Nothing)
                             where
                                openedFileM = retrieveOpenedFile opened filepath
                                  
                                  
retrieveContents :: Handle -> IO [[Char]]
retrieveContents handle = do
                            line <- hGetLine handle
                            isEOF <- hIsEOF handle
                            if isEOF
                                then
                                    return [line]
                                else do
                                    lines <- retrieveContents handle
                                    return (line:lines)
                                      
                                      
retrieveOpenedFile :: [OpenFile] -> FilePath -> Maybe OpenFile
retrieveOpenedFile [] _ = Nothing
retrieveOpenedFile (openfile@(fp, h, iom):ofs) ufp | ufp == fp = Just openfile
                                                   | otherwise = retrieveOpenedFile ofs ufp

                                             
removeOpenedFile :: [OpenFile] -> FilePath -> [OpenFile]
removeOpenedFile [] _ = []
removeOpenedFile (openfile@(fp, h, iom):ofs) ufp | ufp == fp = ofs
                                                 | otherwise = openfile:(removeOpenedFile ofs ufp)

                                           
fileTeardown :: Teardown
fileTeardown sharedIO  fs = do
                             closeAllFiles handles
                             return (sharedIO)
                         where
                             handles = map (\(fp, h, iom) -> h) (opened fs)

closeAllFiles :: [Handle] -> IO ()
closeAllFiles [] = return ()
closeAllFiles (h:hs) = do
                         hClose h
                         closeAllFiles hs