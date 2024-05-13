module Main where
import System.Environment (getArgs)
import Parser (parseL)
import Processor (toSKI, iterateSimplify)
import Printer (cc, unlambda)

main :: IO ()
main = do
  -- read the filename from the argument
  args <- getArgs
  let filename = head args
  -- read the file
  content <- readFile filename
  -- print the content
  let parsed = parseL content
  case parsed of
    Left err -> print err
    Right parsed -> do
      print $ cc $ iterateSimplify $ toSKI parsed
      print $ unlambda $ iterateSimplify $ toSKI parsed
