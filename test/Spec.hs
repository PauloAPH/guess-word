import Lib
import Test.HUnit

-- Define your test cases
test1 :: Test
test1 = TestCase (assertEqual "When everthing matches" "GGGGG" (matchWords "pares" "pares" 0))

test2 :: Test
test2 = TestCase (assertEqual "When everthing is wrong" "RRRRR" (matchWords "boldo" "pares" 0))

test3 :: Test
test3 = TestCase (assertEqual "When everthin in string but wrong place" "YYYYY" (matchWords "respa" "pares" 0))

test4 :: Test
test4 = TestCase (assertEqual "When only the forth letter is wrong" "GGGRG" (matchWords "paris" "pares" 0))

tests :: Test
tests = TestList [TestLabel "test1" test1,TestLabel "test2" test2,TestLabel "test3" test3,TestLabel "test4" test4]

main :: IO Counts
main = runTestTT tests
