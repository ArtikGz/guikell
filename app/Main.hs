module Main where

import Parser
import DefwDefinition
import DefwXlib

frontend :: DefwToken -> IO ()
frontend = defwXlibMain

main :: IO ()
main =
  getLine
    >>= readFile
    >>= (runFrontend . run defwWindow)
  where
    runFrontend (Right (_, window)) = frontend window
    runFrontend (Left err) = putStrLn err
