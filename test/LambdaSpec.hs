{-# LANGUAGE OverloadedStrings #-}

module LambdaSpec where

import Control.Monad
import Lambda
import Test.Hspec

myTests = do
  describe "operator precedence" $ do
    it "\"x\" >: \"x\" @: \"x\" = λx.xx" $ do
      "x" >: "x" @: "x" `shouldBe` (Lam "x" (App (Var "x") (Var "x")))

    it "\"x\" @: \"y\" @: \"z\" = xyz = (xy)z" $ do
      "x" @: "y" @: "z" `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

  describe "β reduction" $ do
    it "β[(λx.x)y]=y" $
      let exp = ("x" >: "x") @: "y"
          res = "y"
       in β exp `shouldBe` res

    it "β[(λx.xx)(λy.yz)]=(λy.yz)(λy.yz)" $
      let exp = ("x" >: "x" @: "x") @: ("x" >: "y" @: "z")
          res = ("x" >: ("y" @: "z")) @: ("x" >: ("y" @: "z"))
       in β exp `shouldBe` res


    forM_
      [ ("(λx.xux)(λx.yz)", "(λx.yz)u(λx.yz)"),
        ("(λx.yz)u(λx.yz)", "yz(λx.yz)"),
        ("(λxy.xy)y", "λz.yz"),
        ("(λxyz.xy)y", "λsz.ys")
      ]
      $ \(strIn, strOut) ->
        it ("β[" ++ strIn ++ "]=" ++ strOut) $
          let (Right exp) = parse strIn
              (Right res) = parse strOut
           in β exp `shouldBe` res

  describe "free" $ do
    it "free[λx.x] = {}" $ do
      let exp = "x" >: "x"
          res = []
       in free exp `shouldBe` res

    it "free[λx.xyxzza] = {a, y, z}" $ do
      let exp = "x" >: "x" @: "y" @: "x" @: "z" @: "z" @: "a"
          res = ["a", "y", "z"]
       in free exp `shouldBe` res

    it "free[λx.x(λz.zxu)y] = {u, y}" $ do
      let exp = "x" >: "x" @: ("z" >: "z" @: "x" @: "u") @: "y"
          res = ["u", "y"]
       in free exp `shouldBe` res
