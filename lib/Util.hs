module Util where

import Data.List
import UTF8
import System.IO
import qualified Data.Set as Set

quote :: String -> String
quote s = ('\"':quot s)
 where quot    [] = "\""
       quot ('\"':xs) = '\\':'\"':quot xs
       quot (x:xs)  = x:quot xs

splitWith :: Eq a => a -> [a] -> Maybe ([a],[a])
splitWith a xs = 
    case (span (/=a) xs) of
      (as,(_:bs)) -> return (as,bs)
      _           -> Nothing

splitList :: Eq a => a -> [a] -> [[a]]
splitList a [] = []
-- splitList 

-- | Print to stderr.
prErr :: String -> IO()
prErr s =  hPutStr stderr (encodeUTF8 (s ++ "\n"))

sort_length l = sortBy f
 where  check xs = or [Set.member s l | s <- xs]
        f x y = case (length x, length y) of
                 (n1,n2) | check x && not (check y) -> LT
                         | not (check x) && check y -> GT
                         | n1 < n2 -> LT
                         | n1 > n2 -> GT
                 _                 -> EQ

sort_length_rev l = sortBy f
 where check xs = or [Set.member s l | s <- xs]
       f x y = case (length x, length y) of
                 (n1,n2) | check x && not (check y) -> GT
                       | not (check x) && check y -> LT
                       | n1 < n2 -> GT
                       | n1 > n2 -> LT
                 _               -> EQ
