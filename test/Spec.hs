import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty(..))
import Parser as P
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testGroup "char parser tests"
    [ testCase "successfully parse a single character" $
      (char 'a') "abc" @?= Right ('a', "bc")

    , testCase "fails to parse a single character" $
      assertFailedParse ((char 'b') "abc")
    ]

  , testCase "parser that always succeeds with value" $
    (always 'z') "abc" @?= Right ('z', "abc")

  , testGroup "zeroOrMore parser tests"
    [ testCase "parses several characters" $
      (zeroOrMore (char 'a')) "aabc" @?= Right ("aa", "bc")

    , testCase "parses nothing but still succeeds" $
      (zeroOrMore (char 'a')) "bcd" @?= Right ("", "bcd")
    ]

  , testGroup "oneOrMore parser tests"
    [ testCase "parses several characters" $
      (oneOrMore (char 'a')) "aabc" @?= Right ('a' :| ['a'], "bc")
    , testCase "fails to parse several one or more characters" $
      assertFailedParse ((oneOrMore (char 'a')) "bcd")
    ]

  , testGroup "either parser tests"
    [ testCase "parses either of two characters (1)" $
      (P.either (char 'a') (char 'b')) "abc" @?= Right ('a', "bc")
    , testCase "parses either of two characters (2)" $
      (P.either (char 'a') (char 'b')) "bac" @?= Right ('b', "ac")
    , testCase "fails to parse either of two characters" $
      assertFailedParse ((P.either (char 'a') (char 'b')) "dab")
    ]

  , testGroup "andThen parser tests"
    [ testCase "successful parse" $
      (char '(' `andThen` char 'a') "(abc" @?= Right ('a', "bc")
    , testCase "unsuccessful parse" $
      assertFailedParse $ (char '(' `andThen` char 'a') "abc"
    ]

  , testCase "test parser mapping" $
    (pmap ord (char 'a')) "abc" @?= Right (97, "bc")
  ]

assertFailedParse :: Either String (a, String) -> Assertion
assertFailedParse (Left _) = pure ()
assertFailedParse (Right _) = assertFailure "Expected a failed parse but it succeeded."
