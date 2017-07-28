module Main where

import Lib
import LogAnalysis
import Log

main :: IO [LogMessage]
main = testParse parse 10 "data/error.log"
--testParse parse 10 "error.log"
