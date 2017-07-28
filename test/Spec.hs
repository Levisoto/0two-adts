module Main where

import Test.Tasty
import Test.Tasty.HUnit

import LogAnalysis
import Log

logAnalysisSuite :: TestTree
logAnalysisSuite = testGroup "Test of Function ParseMessage"
                  [testCase "test 1" $ parseMessage "E 2 562 help help" @?= LogMessage (Error 2) 562 "help help"
                  ,testCase "test 2" $ parseMessage "I 29 la la la" @?= LogMessage Info 29 "la la la"
                  ]

main = defaultMain logAnalysisSuite


--main :: IO ()
--main = putStrLn "Hola me llamo levi Y voy a ser el mejor"


