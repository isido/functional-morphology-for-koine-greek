module Guessing (silly_guesser) where

silly_guesser :: String -> [String]
silly_guesser s 
 | last s == 'x' = ["First silly guess", "Second guess"]
 | otherwise     = [] -- Unable to guess anything
