module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Block1.Task1Spec (weekdayTestTree)
import Block1.Task2Spec (natTestTree)
import Block1.Task3Spec (treeTestTree)
import Block2.Task1Prop (treeFoldableTestTree)
import Block2.Task2Spec (splitJoinTestTree)
import Block3.Task1Spec (maybeEitherConcatTestTree)
import Block3.Task2Spec (monoidsCheckTestTree)
import Block4.Task1Spec (stringSumTestTree)
import Block5.Task1Spec (evalExprTestTree)
import Block6.Task1Spec (parserInstancesTestTree)
import Block6.Task2Spec (primitiveCombinatorsTestTree)
import Block6.Task3Spec (simpleParsersTestTree)
import Block6.Task4Spec (listListParserTestTree)

main :: IO ()
main = do
  b1t1 <- weekdayTestTree
  b1t2 <- natTestTree
  b1t3 <- treeTestTree

  b2t1 <- treeFoldableTestTree
  b2t2 <- splitJoinTestTree

  b3t1 <- maybeEitherConcatTestTree
  b3t2 <- monoidsCheckTestTree

  b4t1 <- stringSumTestTree

  b5t1 <- evalExprTestTree

  b6t1 <- parserInstancesTestTree
  b6t2 <- primitiveCombinatorsTestTree
  b6t3 <- simpleParsersTestTree
  b6t4 <- listListParserTestTree

  defaultMain $ testGroup "All" [ testGroup "Block1" [b1t1, b1t2, b1t3]
                                , testGroup "Block2" [b2t1, b2t2]
                                , testGroup "Block3" [b3t1, b3t2]
                                , testGroup "Block4" [b4t1]
                                , testGroup "Block5" [b5t1]
                                , testGroup "Block6" [b6t1, b6t2, b6t3, b6t4]
                                ]
