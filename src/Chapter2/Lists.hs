module Chapter2.Lists where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_:ys) = xs : suffixes ys