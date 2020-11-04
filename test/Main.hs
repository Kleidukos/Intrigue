module Main (main) where
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import qualified Test.Parsing as Parsing
import qualified Test.Evaluating as Evaluating

main :: IO ()
main = do
    parsingSpec <- testSpec "Parsing" Parsing.spec
    evaluatingSpec <- testSpec "Evaluating" Evaluating.spec
    Test.Tasty.defaultMain
      (testGroup "Intrigue"
        [ parsingSpec
        , evaluatingSpec
        ]
      )

