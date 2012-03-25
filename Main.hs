module Main where

import           Layout (chart)

main :: IO ()
main = writeFile "index.html" (chart ++ "\n")
