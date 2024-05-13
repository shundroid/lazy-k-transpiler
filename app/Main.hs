module Main where
import System.Environment (getArgs)
import Parser (parseL)
import Processor (toSKI, iterateSimplify)
import Printer (cc, unlambda)
import Reduction (runReduction)

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
      print parsed
      let reduced = runReduction parsed
      print reduced
      let skied = toSKI reduced
      let simplified = iterateSimplify skied
      print $ cc simplified
      print $ unlambda simplified
      -- check
      print $ runReduction simplified
