module Main
   ( main
   ) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import qualified Test.Control.Algebra.Free (tests)
import qualified Test.Data.Algebra.Free (tests)
import qualified Test.Data.Group.Free (tests)
import qualified Test.Data.Semigroup.SSet (tests)
import qualified Test.Data.Monoid.MSet (tests)

runTests :: [IO Bool] -> IO ()
runTests tests = do
    res <- and <$> sequence tests
    unless res
        exitFailure

main :: IO ()
main = do
    runTests
        [ Test.Control.Algebra.Free.tests
        , Test.Data.Algebra.Free.tests
        , Test.Data.Group.Free.tests
        , Test.Data.Semigroup.SSet.tests
        , Test.Data.Monoid.MSet.tests
        ]
