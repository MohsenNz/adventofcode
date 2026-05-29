module Day4Tests (tests) where

import Day4
import GHC.Stack (HasCallStack)
import Test.HUnit

assertEq_
    :: (HasCallStack, Eq a, Show a)
    => a
    -> a
    -> Assertion
assertEq_ = assertEqual ""

testEvalLine :: Test
testEvalLine = TestCase $ assertEq_ 2 $ evalLine "XMASAMXAMM"

testUnzipColumn0_1 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "123"
        , "456"
        , "789"
        ]
    f = unzipColumn0
    a@(_, ds2) = f ds1
    b@(_, ds3) = f ds2
    c = f ds3
    result = [a, b, c]
    expect =
        [ ("741", ["89", "56", "23"])
        , ("258", ["3", "6", "9"])
        , ("963", [])
        ]

testUnzipColumn0_2 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "123"
        , "456"
        ]
    f = unzipColumn0
    a@(_, ds2) = f ds1
    b@(_, ds3) = f ds2
    c = f ds3
    result = [a, b, c]
    expect =
        [ ("41", ["56", "23"])
        , ("25", ["3", "6"])
        , ("63", [])
        ]

testUnzipColumn0_3 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "12"
        , "45"
        , "78"
        ]
    f = unzipColumn0
    a@(_, ds2) = f ds1
    b = f ds2
    result = [a, b]
    expect =
        [ ("741", ["8", "5", "2"])
        , ("258", [])
        ]

testUnzipDiagonalR0_1 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "123"
        , "456"
        , "789"
        ]
    f = unzipDiagonalR0 3 3
    a@(_, ds2) = f ds1 1
    b@(_, ds3) = f ds2 2
    c@(_, ds4) = f ds3 3
    d@(_, ds5) = f ds4 4
    e = f ds5 5
    result = [a, b, c, d, e]
    expect =
        [ ("1", ["23", "456", "789"])
        , ("42", ["3", "56", "789"])
        , ("753", ["6", "89"])
        , ("86", ["9"])
        , ("9", [])
        ]

testUnzipDiagonalR0_2 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "123"
        , "456"
        ]
    f = unzipDiagonalR0 3 2
    a@(_, ds2) = f ds1 1
    b@(_, ds3) = f ds2 2
    c@(_, ds4) = f ds3 3
    d = f ds4 4
    result = [a, b, c, d]
    expect =
        [ ("1", ["23", "456"])
        , ("42", ["3", "56"])
        , ("53", ["6"])
        , ("6", [])
        ]

testUnzipDiagonalR0_3 = TestCase $ assertEq_ expect result
  where
    ds1 =
        [ "12"
        , "45"
        , "78"
        ]
    f = unzipDiagonalR0 2 3
    a@(_, ds2) = f ds1 1
    b@(_, ds3) = f ds2 2
    c@(_, ds4) = f ds3 3
    d = f ds4 4
    result = [a, b, c, d]
    expect =
        [ ("1", ["2", "45", "78"])
        , ("42", ["5", "78"])
        , ("75", ["8"])
        , ("8", [])
        ]

tests :: Test
tests =
    TestList
        [ testEvalLine
        , testUnzipColumn0_1
        , testUnzipColumn0_2
        , testUnzipColumn0_3
        , testUnzipDiagonalR0_1
        , testUnzipDiagonalR0_2
        , testUnzipDiagonalR0_3
        ]

-- f :: [Line] -> [(Line, [Line])]
-- f dat = go [] dat 1
--   where
--     go acc [] _ = acc
--     go acc dat i =
--         let x@(_, dat_) = unzipDiagonalR0 width height dat i
--          in go (x : acc) dat_ (i + 1)
--
--     width = length $ head dat
--     height = length dat

-- test2 :: Test
-- test2 = TestCase (assertEqual "-1 + 1 should be 0" 0 (add (-1) 1))
