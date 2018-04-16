module Main where

import Lib
import Prelude
import System.Environment (getArgs)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

main :: IO ()
main = do
  args <-  getArgs
  let title = safeHead args
  let grades = safeHead $ tail args
  let template = safeHead $ tail (tail args)
  runGradeStudents title grades template

