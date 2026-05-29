{-# LANGUAGE ImportQualifiedPost #-}

import Day4Tests qualified
import Test.HUnit

main :: IO ()
main = do
    counts <- runTestTT Day4Tests.tests
    if failures counts > 0 || errors counts > 0
        then error "Some tests failed"
        else return ()
