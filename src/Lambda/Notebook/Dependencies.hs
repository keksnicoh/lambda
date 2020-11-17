{-# LANGUAGE MultiParamTypeClasses #-}

module Lambda.Notebook.Dependencies where

class HasM t m where
  getM :: m t
