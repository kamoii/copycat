module Main where

import Prelude()
import Relude
import Lib

main :: IO ()
main = absurd <$> copycat
