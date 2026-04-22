-- CLI ENTRY POINT
-- READ PROGRAM FILE FROM COMMAND LINE
-- PARSE INTO-- PRINT ONLY STDOUT RESULT
module Main where

import System.Environment (getArgs)
import qualified Parser
import qualified Eval
import qualified Render

main :: IO ()
main = do
  args <- getArgs
  case args of
    [programFile] -> do
      src <- readFile programFile
      let query = Parser.parseProgram src
      triples <- Eval.runQuery query
      putStr (Render.renderGraph triples)
    _ -> error "Usage: terrapin <program-file>"