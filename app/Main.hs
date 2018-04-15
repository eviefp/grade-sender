module Main where

import Lib

main :: IO ()
main = do
  mgf <- parseGradeFiles "../marks.csv"
  case mgf of
    Nothing -> putStrLn "Parse error"
    Just gf -> do
      let prs = fmap parse gf
      doGradeMail "smtp.gmail.com" prs
