module Spec
   ( main
   ) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import qualified Test.Algebra.Free.Class (tests)
import qualified Test.Algebra.Free.Higher (tests)

runTests :: [IO Bool] -> IO ()
runTests tests = do
    res <- and <$> sequence tests
    unless res
        exitFailure

main :: IO ()
main = do
    runTests
        [ Test.Algebra.Free.Class.tests
        , Test.Algebra.Free.Higher.tests
        ]
