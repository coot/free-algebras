module Main
   ( main
   ) where

import           Control.Monad (unless)
import           System.IO (hSetEncoding, stdout, utf8)
import           System.Exit (exitFailure)

import qualified Test.Control.Algebra.Free (tests)
import qualified Test.Data.Algebra.Free (tests)
import qualified Test.Data.Group.Free (tests)

runTests :: [IO Bool] -> IO ()
runTests tests = do
    res <- and <$> sequence tests
    unless res
        exitFailure

main :: IO ()
main = do
    hSetEncoding stdout utf8
    runTests
        [ Test.Control.Algebra.Free.tests
        , Test.Data.Algebra.Free.tests
        , Test.Data.Group.Free.tests
        ]
