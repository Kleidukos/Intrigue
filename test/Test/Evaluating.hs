module Test.Evaluating where

import Test.Tasty.Hspec

import Intrigue.Types
import Intrigue

spec :: Spec
spec = parallel $ do
  describe "No change for self-evaluating data structures" $ do
    it "'(1 2 '(3 4))" $ do
      (prettyPrint <$> evalProgram "'(1 2 '(3 4))")
        `shouldReturn` "'(1 2 '(3 4))"
  describe "Numerical operations" $ do
    it "(+)" $
      (prettyPrint <$> evalProgram "(+ 1 2 3 4)")
        `shouldReturn` "10"
    it "(-)" $
      (prettyPrint <$> evalProgram "(- 4 3 2 1)")
        `shouldReturn` "-2"
    it "(number?)" $
      (prettyPrint <$> evalProgram "(number? 3)")
        `shouldReturn` "#t"
    it "(=)" $
      (prettyPrint <$> evalProgram "(= 3 3 3 3)")
        `shouldReturn` "#t"
    it "(>)" $
      (prettyPrint <$> evalProgram "(> 4 3 2 1)")
        `shouldReturn` "#t"
    it "(<)" $
      (prettyPrint <$> evalProgram "(< 4 3 2 1)")
        `shouldReturn` "#f"
    it "(>=)" $
      (prettyPrint <$> evalProgram "(>= 4 3 2 4)")
        `shouldReturn` "#f"
