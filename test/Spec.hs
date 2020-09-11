import Data.List.NonEmpty (NonEmpty(..))
import Parser as P
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testCase "successfully parse a single character" $
    (char 'a') "abc" @?= Right ('a', "bc")
  , testCase "fails to parse a single character" $
    assertFailedParse ((char 'b') "abc")
  , testCase "parser that always succeeds with value" $
    (always 'z') "abc" @?= Right ('z', "abc")
  , testCase "parses several characters" $
    (zeroOrMore (char 'a')) "aabc" @?= Right ("aa", "bc")
  , testCase "parses nothing but still succeeds" $
    (zeroOrMore (char 'a')) "bcd" @?= Right ("", "bcd")
  , testCase "parses several characters" $
    (oneOrMore (char 'a')) "aabc" @?= Right ('a' :| ['a'], "bc")
  , testGroup "either parser tests"
    [ testCase "parses either of two characters (1)" $
      (P.either (char 'a') (char 'b')) "abc" @?= Right ('a', "bc")
    , testCase "parses either of two characters (2)" $
      (P.either (char 'a') (char 'b')) "bac" @?= Right ('b', "ac")
    , testCase "fails to parse either of two characters" $
      assertFailedParse ((P.either (char 'a') (char 'b')) "dab")
    ]
  ]

assertFailedParse :: Either String (a, String) -> Assertion
assertFailedParse (Left _) = pure ()
assertFailedParse (Right _) = assertFailure "Expected a failed parse but it succeeded."
