module Eventloop.Module.File.Types where

import System.IO

type OpenFile = (FilePath, Handle, IOMode)

data FileIn = FileOpened FilePath Bool
            | FileClosed FilePath Bool
            | RetrievedContents FilePath [[Char]]
            | RetrievedLine FilePath [Char]
            | RetrievedChar FilePath Char
            | IsEOF FilePath Bool
            | WroteTo FilePath Bool
            deriving (Eq, Show)

data FileOut = OpenFile FilePath IOMode
             | CloseFile FilePath
             | RetrieveContents FilePath
             | RetrieveLine FilePath
             | RetrieveChar FilePath
             | IfEOF FilePath
             | WriteTo FilePath [Char]
             deriving (Eq, Show)