module Block5.Task1Spec
 ( evalExprTestTree
 ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

import Block5.Task1 (ArithmeticError(..), Expr(..), eval)

evalExprTestTree :: IO TestTree
evalExprTestTree = testSpec "Monadic Expr and eval" evalExprSpec

evalExprSpec :: Spec
evalExprSpec = do
  it "const" $
    eval (Const 5) `shouldBe` Right 5
  it "add" $
    eval (Add (Const 4) (Const 3)) `shouldBe` Right 7
  it "sub" $
    eval (Sub (Const 3) (Const 8)) `shouldBe` Right (-5)
  it "mul" $
    eval (Mul (Const 2) (Const (-4))) `shouldBe` Right (-8)
  it "div" $
    eval (Div (Const 7) (Const 3)) `shouldBe` Right 2
  it "pow" $
    eval (Pow (Const 3) (Const 2)) `shouldBe` Right 9
  it "div fail" $
    eval (Div (Const 3) (Const 0)) `shouldBe`
      Left (ArithmeticError "Division by zero")
  it "pow fail" $
    eval (Pow (Const 2) (Const (-1))) `shouldBe`
      Left (ArithmeticError "Negative exponent")