{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           AesonImpl
import           CoreDataImpl

import           Core.Program

version :: Version
version = $(fromPackage)

argConfig :: Config
argConfig = complex
    [ Command "core-data"
              "Use core-data implementation."
              [Argument "filename" "JSON file to read."]
    , Command "aeson"
              "Use aeson implementation"
              [Argument "filename" "JSON file to read."]
    ]

selectProgram :: Maybe LongName -> Maybe FilePath -> Program None ()
selectProgram _                  Nothing     = return ()
selectProgram (Just "core-data") (Just path) = runCoreData path
selectProgram (Just "aeson"    ) (Just path) = runAeson path
selectProgram _                  _           = return ()

program :: Program None ()
program = do
    params <- getCommandLine
    let cmd  = commandNameFrom params
    let path = lookupArgument "filename" params
    selectProgram cmd path

main :: IO ()
main = do
    context <- configure version None argConfig
    executeWith context program
