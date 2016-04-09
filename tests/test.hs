import           Test.Tasty
import           Test.Tasty.HUnit

import           HOFuncs
import           ListCompr
import           Parsers
import           RecurFuncs

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [listComprTests, recurFuncsTest, hoFuncsTest, parsersTest]

listComprTests = testGroup "List Comprehension"
  [ testCase "flatten" $
      flatten [[1,2], [3,4]] @=? [1, 2, 3, 4]
  ]

recurFuncsTest = testGroup "Recur Funcs"
  [ testCase "factorial using list compr" $
      factorial 4 @=? 24
  , testCase "factorial using recursion" $
      factorial' 4 @=? 24
  , testCase "product using recursion" $
    product' [1,2,3,4] @=? 24
  ]

hoFuncsTest = testGroup "Higher Oder Funcs"
  [ testCase "odd" $
    odd' 5 @=? True
  ]

parsersTest = testGroup "Parsers"
  [ testCase "str" $
      parse item "abc" @=? [('a', "bc")]
  , testCase "return" $
      parse (Parsers.return 1) "abc" @=? [(1, "abc")]
  , testCase "empty str" $
      parse item "" @=? []
  ]
