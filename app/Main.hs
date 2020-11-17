{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Except
  ( runExceptT,
  )
import Control.Monad.State (StateT (..))
import Control.Monad.Writer (WriterT (..))
import qualified Data.Map as M
import Lambda.Lib.Language ( eval, handler, parse )
import LangT ( LangT(runLangT) )

main :: IO ()
main = evalFile "examples/resolve.txt"

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