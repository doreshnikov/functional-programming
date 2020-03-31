module Block6.Task4Spec
 ( listListParserTestTree
 ) where

import Control.Applicative (empty, (<|>))
import Control.Exception (evaluate)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, it, shouldBe, shouldThrow, testSpec)

import Block6.Task1 (runParser)
import Block6.Task4 (listListParser)
import Block6.Utils (extract)

listListParserTestTree :: IO TestTree
listListParserTestTree = testSpec "complex listList parser" listListParserSpec

listListParserSpec :: Spec
listListParserSpec = do
  it "<empty>" $
    extract (runParser listListParser "") `shouldBe` []
  it "one empty list" $
    extract (runParser listListParser "0") `shouldBe` [[]]
  it "one list one value" $
    extract (runParser listListParser "1, 3") `shouldBe` [[3]]
  it "complex (example)" $
    extract (runParser listListParser "2, 1,+10  , 3,5,-7, 2") `shouldBe` [[1, 10], [5, -7, 2]]
  it "complex (another)" $
    extract (runParser listListParser "  2, 1,+10 ,0 , 1,-0  ") `shouldBe` [[1, 10], [], [-0]]
  it "extra comma (begin)" $
    evaluate (extract $ runParser listListParser ", 2, 1, 10") `shouldThrow` anyErrorCall
  it "extra comma (middle)" $
    evaluate (extract $ runParser listListParser "2, , 1, 10") `shouldThrow` anyErrorCall
  it "extra comma (end)" $
    evaluate (extract $ runParser listListParser "2, 1, 10, ") `shouldThrow` anyErrorCall
  it "missing comma" $
    evaluate (extract $ runParser listListParser "2 1, 10") `shouldThrow` anyErrorCall
  it "negative size" $
    evaluate (extract $ runParser listListParser "-1, 1") `shouldThrow` anyErrorCall
  it "invalid size" $
    evaluate (extract $ runParser listListParser "3, 1, 10") `shouldThrow` anyErrorCall
  it "invalid chars" $
    evaluate (extract $ runParser listListParser "3, 1x, 10") `shouldThrow` anyErrorCall
