module Eventloop.Module.File.File
    ( setupFileModuleConfiguration
    , fileModuleIdentifier
    , fileEventRetriever
    , fileEventSender
    , fileTeardown
    ) where

import Data.Maybe
import Control.Concurrent.Datastructures.BlockingConcurrentQueue
import Control.Concurrent.STM
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
fileInitializer sharedConst sharedIO
    = do
        inQueue <- createBlockingConcurrentQueue
        return (sharedConst, sharedIO, FileConstants inQueue, FileState [])

    
fileEventRetriever :: EventRetriever
fileEventRetriever sharedConst sharedIOT ioConst ioStateT
    = do
        fileInEvents <- takeAllFromBlockingConcurrentQueue queue
        return (map InFile fileInEvents)
    where
        queue = fileInQueue ioConst


fileEventSender :: EventSender
fileEventSender sharedConst sharedIOT ioConst ioStateT (OutFile out)
    = do
        (FileState openFiles) <- readTVarIO ioStateT
        (openFiles', inEvents) <- fileEventSender' openFiles out
        atomically $ writeTVar ioStateT (FileState openFiles')
        putAllInBlockingConcurrentQueue inQueue inEvents
    where
        inQueue = fileInQueue ioConst

    
fileEventSender' :: [OpenFile] -> FileOut -> IO ([OpenFile], [FileIn])
fileEventSender' openFiles (OpenFile filepath iomode)
    = do
        handle <- openFile filepath iomode
        let
            fileOpenedEvent = FileOpened filepath True
            openFiles' = openFiles ++ [(filepath, handle, iomode)]
        return (openFiles', [fileOpenedEvent])
                                                                                         
fileEventSender' openFiles (CloseFile filepath)
    | openFileM == Nothing = return ([], [])
    | otherwise = do
                   hClose handle
                   return (openFiles', [closedFileEvent])
    where
        openFileM = retrieveOpenedFile openFiles filepath
        (fp, handle, iomode) = fromJust openFileM
        openFiles' = removeOpenedFile openFiles filepath
        closedFileEvent = FileClosed filepath True
                                                        
fileEventSender' openFiles (RetrieveContents filepath)
    = doReadAction filepath openFiles RetrievedContents retrieveContents

fileEventSender' openFiles (RetrieveLine filepath)
     = doReadAction filepath openFiles RetrievedLine hGetLine

fileEventSender' openFiles (RetrieveChar filepath)
    = doReadAction filepath openFiles RetrievedChar hGetChar

fileEventSender' openFiles (IfEOF filepath)
    = getFromFile filepath openFiles fileIsOpened IsEOF hIsEOF
                                                                     
fileEventSender' openFiles (WriteTo filepath contents)
    | fileIsWriteable openFiles filepath = do
        hPutStr handle contents
        return (openFiles, [WroteTo filepath True])
    | otherwise = return (openFiles, [])
    where
        Just (fp, handle, iomode) = retrieveOpenedFile openFiles filepath

                                                                                
doReadAction :: FilePath
             -> [OpenFile]
             -> (FilePath -> a -> FileIn)
             -> (Handle -> IO a)
             -> IO ([OpenFile], [FileIn])
doReadAction filepath openFiles inEvent readAction
    = getFromFile filepath openFiles fileIsReadable inEvent readAction
                         
                         
getFromFile :: FilePath -> 
               [OpenFile] ->
               ([OpenFile] -> FilePath -> Bool) -> {- Check if the action should be done -}
               (FilePath -> a -> FileIn) -> {- The inEvent Constructor -}
               (Handle -> IO a) ->  {- The action which will grant a result -}
               IO ([OpenFile], [FileIn])
getFromFile filepath openFiles fileCheck inEvent action
    | fileCheck openFiles filepath = do
        result <- action handle
        return (openFiles, [inEvent filepath result])
    | otherwise                 = return (openFiles, [])
    where
        Just (fp, handle, iomode) = retrieveOpenedFile openFiles filepath
                                                            

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
fileTeardown sharedConst sharedIO ioConst ioState
    = do
         closeAllFiles handles
         return (sharedIO)
    where
        handles = map (\(fp, h, iom) -> h) (opened ioState)


closeAllFiles :: [Handle] -> IO ()
closeAllFiles [] = return ()
closeAllFiles (h:hs) = do
                         hClose h
                         closeAllFiles hs