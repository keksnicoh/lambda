{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (hspec)
import qualified LambdaSpec

main :: IO ()
main = hspec $ do
    LambdaSpec.myTests

