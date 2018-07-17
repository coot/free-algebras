module Spec
   ( main
   ) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import qualified Test.Data.Algebra.Free (tests)
import qualified Test.Control.Algebra.Free (tests)

runTests :: [IO Bool] -> IO ()
runTests tests = do
    res <- and <$> sequence tests
    unless res
        exitFailure

main :: IO ()
main = do
    runTests
        [ Test.Data.Algebra.Free.tests
        , Test.Control.Algebra.Free.tests
        ]
