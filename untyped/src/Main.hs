module Main where

import qualified Data.Map as Map
import Lambda.Interp (interp, lambSum)

main :: IO ()
main = print $ interp (lambSum 210 210) Map.empty
