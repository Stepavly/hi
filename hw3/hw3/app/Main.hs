module Main
  (
    main
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import HW3.Action
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

loop :: InputT IO ()
loop = do
  inp <- getInputLine "hi> "
  case inp of
    Nothing -> return ()
    Just inp' -> do
      case parse inp' of
        Left err -> outputStrLn (show err)
        Right expr -> do
          evalRes <- liftIO (eval expr)
          case evalRes of
            Left err  -> outputStrLn $ "Evaluation error: " ++ show err
            Right val -> outputStrLn $ show $ prettyValue val
      loop

main :: IO ()
main = runInputT defaultSettings loop
