import Game 

import Graphics.Gloss
import Test.HUnit

test0 :: Test
test0 = TestCase (assertEqual "When the letter is in the right place" green (letterInWord 'a' "aeiou" 0))

test1 :: Test
test1 = TestCase (assertEqual "When the letter in the word" yellow (letterInWord 'a' "aeiou" 1))

test2 :: Test
test2 = TestCase (assertEqual "When the letter is not in the word" red (letterInWord 'b' "aeiou" 0))


tests :: Test
tests = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2]

main :: IO Counts
main = runTestTT tests
