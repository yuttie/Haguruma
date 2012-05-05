import Test.Framework as Test
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Haguruma hiding (defaultMain)


main = defaultMain tests

tests = [ testGroup "Schedule ABCD" [
            testCase "scheduleA" case_scheduleA,
            testCase "scheduleB" case_scheduleB,
            testCase "scheduleC" case_scheduleC,
            testCase "scheduleD" case_scheduleD
          ]
        , testGroup "Schedule Nums" [
            testCase "schedule2"  case_schedule2,
            testCase "schedule3"  case_schedule3,
            testCase "schedule5"  case_schedule5,
            testCase "schedule7"  case_schedule7,
            testCase "schedule8"  case_schedule8,
            testCase "schedule9"  case_schedule9,
            testCase "schedule10" case_schedule10,
            testCase "schedule11" case_schedule11
          ]
        ]

case_scheduleA = schedule dlABCD [stepA] @?= [stepC, stepD, stepA]
case_scheduleB = schedule dlABCD [stepB] @?= [stepC, stepD, stepA, stepB]
case_scheduleC = schedule dlABCD [stepC] @?= [stepC]
case_scheduleD = schedule dlABCD [stepD] @?= [stepC, stepD]

case_schedule2  = schedule dlNums [step2 ] @?= [step7, step5, step11, step2]
case_schedule3  = schedule dlNums [step3 ] @?= [step3]
case_schedule5  = schedule dlNums [step5 ] @?= [step5]
case_schedule7  = schedule dlNums [step7 ] @?= [step7]
case_schedule8  = schedule dlNums [step8 ] @?= [step7, step3, step8]
case_schedule9  = schedule dlNums [step9 ] @?= [step7, step5, step11, step3, step8, step9]
case_schedule10 = schedule dlNums [step10] @?= [step7, step5, step11, step3, step10]
case_schedule11 = schedule dlNums [step11] @?= [step7, step5, step11]


stepA = Step { name = "A"
             , inputs = ["c1", "d2"]
             , outputs = ["a1"]
             , action = do writeFile "a1" "a"
             }
stepB = Step { name = "B"
             , inputs = ["a1", "c1"]
             , outputs = ["b1", "b2"]
             , action = do writeFile "b1" "bb"
                           writeFile "b2" "bbbb"
             }
stepC = Step { name = "C"
             , inputs = []
             , outputs = ["c1"]
             , action = do writeFile "c1" "ccc"
             }
stepD = Step { name = "D"
             , inputs = ["c1"]
             , outputs = ["d1", "d2"]
             , action = do writeFile "d1" "dddd"
                           writeFile "d2" "dddddddd"
             }
dlABCD :: DepList Step
dlABCD = dependencyList [stepA, stepB, stepC, stepD]


step2 = Step { name = "2"
             , inputs = ["11"]
             , outputs = ["2"]
             , action = do writeFile "2" "2"
             }
step3 = Step { name = "3"
             , inputs = []
             , outputs = ["3"]
             , action = do writeFile "3" "3"
             }
step5 = Step { name = "5"
             , inputs = []
             , outputs = ["5"]
             , action = do writeFile "5" "5"
             }
step7 = Step { name = "7"
             , inputs = []
             , outputs = ["7"]
             , action = do writeFile "7" "7"
             }
step8 = Step { name = "8"
             , inputs = ["7", "3"]
             , outputs = ["8"]
             , action = do writeFile "8" "8"
             }
step9 = Step { name = "9"
             , inputs = ["11", "8"]
             , outputs = ["9"]
             , action = do writeFile "9" "9"
             }
step10= Step { name = "10"
             , inputs = ["11", "3"]
             , outputs = ["10"]
             , action = do writeFile "10" "10"
             }
step11= Step { name = "11"
             , inputs = ["7", "5"]
             , outputs = ["11"]
             , action = do writeFile "11" "11"
             }
dlNums :: DepList Step
dlNums = dependencyList [step2, step3, step5, step7, step8, step9, step10, step11]
