-- DateTime REPL

import           DateTime
import           Data.List                (isPrefixOf)
import           System.Console.Haskeline
import           Control.Monad.IO.Class (liftIO)

main :: IO ()
main = putStrLn description >> runInputT settings loop
  where
    settings = defaultSettings { historyFile = Just ".datetime-hist" }

    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just ""      -> loop
        Just s | s `isPrefixOf` ":quit"  -> return ()
               | s `isPrefixOf` ":help"  -> (outputStrLn $ helpMsg) >> loop
               | s `isPrefixOf` ":zones" -> (outputStrLn $ zonesList) >> loop
        Just input   -> do
          -- https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad-IO-Class.html#v:liftIO
          result <- liftIO (datetime input)
          outputStrLn result
          loop
