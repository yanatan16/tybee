module Test.Test where

import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, NonNegative(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.HUnit (assertBool)

import Trading.Tybee ()

-----------------
---- Tests
-----------------

-----------------
---- List of Tests
-----------------

tests :: [Test]
tests = [
  ]

-- convenient whilst in ghci
runAllTests :: IO ()
runAllTests = defaultMainWithArgs tests []

runTests :: String -> IO ()
runTests p = defaultMainWithArgs tests ["--select-tests", p]

runGroup :: Int -> IO ()
runGroup i = defaultMainWithArgs [tests !! i] []