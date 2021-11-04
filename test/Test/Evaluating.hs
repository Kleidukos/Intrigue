module Test.Evaluating where

import Data.Text.Display
import Test.Hspec

import Intrigue

spec :: Spec
spec = parallel $ do
  describe "Quote" $ do
    it "(car '(1 2 3))" $ do
      (display <$> evalProgram "(car '(1 2 3))")
        `shouldReturn` "1"
  describe "Numerical operations" $ do
    it "(+)" $
      (display <$> evalProgram "(+ 1 2 3 4)")
        `shouldReturn` "10"
    it "(-)" $
      (display <$> evalProgram "(- 4 3 2 1)")
        `shouldReturn` "-2"
    it "(number?)" $
      (display <$> evalProgram "(number? 3)")
        `shouldReturn` "#t"
    it "(=)" $
      (display <$> evalProgram "(= 3 3 3 3)")
        `shouldReturn` "#t"
    it "(>)" $
      (display <$> evalProgram "(> 4 3 2 1)")
        `shouldReturn` "#t"
    it "(<)" $
      (display <$> evalProgram "(< 4 3 2 1)")
        `shouldReturn` "#f"
    it "(>=)" $
      (display <$> evalProgram "(>= 4 3 2 4)")
        `shouldReturn` "#f"
