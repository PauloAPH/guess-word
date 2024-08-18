import Lib
import Test.HUnit

test0 :: Test
test0 = TestCase (assertEqual "When the letter is in the right place" "G" (letterInWord 'a' "aeiou" 0))

test1 :: Test
test1 = TestCase (assertEqual "When the letter in the word" "Y" (letterInWord 'a' "aeiou" 1))

test2 :: Test
test2 = TestCase (assertEqual "When the letter is not in the word" "R" (letterInWord 'b' "aeiou" 0))

test3 :: Test
test3 = TestCase (assertEqual "When everthing matches" "GGGGG" (matchWords "pares" "pares" 0))

test4 :: Test
test4 = TestCase (assertEqual "When everthing is wrong" "RRRRR" (matchWords "boldo" "pares" 0))

test5 :: Test
test5 = TestCase (assertEqual "When everthin in string but wrong place" "YYYYY" (matchWords "respa" "pares" 0))

test6 :: Test
test6 = TestCase (assertEqual "When only the forth letter is wrong" "GGGRG" (matchWords "paris" "pares" 0))

tests :: Test
tests = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6]

main :: IO Counts
main = runTestTT tests
