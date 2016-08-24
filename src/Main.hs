
-- import qualified Control.Exception as Exn
import System.IO (hFlush, stdout)

import qualified Data.Map as Map

import Ast
import Parser
import Printer
import Krivine

readPrompt :: String -> IO String
readPrompt prompt = do
  putStr prompt
  hFlush stdout
  getLine

main :: IO ()
main = do
  str <- readPrompt "λ→ "
  case parser (Map.empty) str of
    Left  e -> putStrLn $ "Parse error: " ++ show e
    -- Right e -> mapM_ print $ Krivine.states e
    Right e -> do
      putStrLn $ "Parsing:      " ++ show (e :: Term ())
      putStrLn $ "Simple Eval:  " ++ show (eval undefined e)
  main -- loop
