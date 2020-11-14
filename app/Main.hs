{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadIO,
    runExceptT,
  )
import Control.Monad.State (MonadState, StateT (..), gets, modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT (..))
import qualified Data.Map as M
import Language
import LangT

main :: IO ()
main = evalFile "test5.txt"

evalFile :: FilePath -> IO ()
evalFile filePath = do
  putStrLn "========================================================"
  putStrLn $ "run " ++ filePath
  content <- readFile filePath
  putStrLn "========================================================"
  putStrLn content
  putStrLn "========================================================"
  case parse content of
    Right lng ->
      runExceptT (runStateT (runWriterT (runLangT (eval handler lng))) M.empty) >>= \case
        Right ((_, stdOut), _) -> forM_ stdOut putStrLn
        Left str -> error str
    Left x -> error $ show x