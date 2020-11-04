module Test.Evaluating where

import Test.Tasty.Hspec

import Intrigue.Types
import Intrigue

spec :: Spec
spec = parallel $ do
  describe "No change for self-evaluating data structures" $ do
    it "(1 2 '(3 4))" $ do
      (prettyPrint <$> evalProgram "(1 2 '(3 4))")
        `shouldReturn` "(1 2 '(3 4))"
