module EditDistance where

import Data.Array
import Data.List
import Data.Char
import System.IO -- TODO: check
import qualified Data.Set

data Position    = Int

data Operation a = Delete      a | 
		   Insert      a | 
		   Replace   a a | 
		   Untouched   a
  deriving (Show,Eq)

isUntouched :: Operation a -> Bool
isUntouched (Untouched _) = True
isUntouched _             = False

isOp :: Operation a -> Bool
isOp = not . isUntouched
                
-- Compares a list of strings, and gives a set of common substrings
commonSubsequences :: Ord a => [[a]] -> Data.Set.Set [a]
commonSubsequences      []  = Data.Set.empty
commonSubsequences     [x]  = Data.Set.empty
commonSubsequences (x:y:xs) = Data.Set.fromList $ traverse (commonSub x y) xs
 where traverse x []        = x
       traverse x (y:ys)    = traverse (commonSub (concat x) y) ys

-- Compares two strings, and gives a list of common substrings,
-- not necessary unique
commonSub :: Eq a => [a] -> [a] -> [[a]]
commonSub s1 s2 = collect $ snd $ editDistance s1 s2
 where collect [] = []
       collect (x:xs)
        | isUntouched x = case span isUntouched (x:xs) of
			   (us,rest) -> [c | (Untouched c) <- us] : collect rest
	| otherwise     = collect xs

-- Calculate the edit distance of two strings. 
-- The result is the edit distance metrics and a list of operations.
editDistance ::  Eq a => [a] -> [a] -> (Int,[Operation a])
editDistance s1 s2 = (tab ! (l1,l2), reverse (backtrack tab (l1,l2) (s1a,s2a)))
 where l1 = length s1
       l2 = length s2
       s1a = array (0,l1) (zip [1..(l1+1)] s1)
       s2a = array (0,l2) (zip [1..(l2+1)] s2)
       tab = array ((0,0),(l1,l2)) 
	      [((x,y),edit (x,y)) | x <- [0..l1], y <- [0..l2]]
       edit (x,0) = x
       edit (0,x) = x
       edit (x,y) = 
	   minimum [tab ! (x-1,y)+1,          -- insertion
		    tab ! (x,y-1)+1,          -- deletion
	            tab ! (x-1,y-1)+cost x y] -- substitution
       cost x y 
         | s1a ! x == s2a ! y = 0
	 | otherwise          = 2 -- substitution cost.

dot_it :: [String] -> String
dot_it (s:xs) = dot s $ map (snd . (editDistance s)) xs

-- Trace the edit distance algorithm
backtrack :: Array (Int,Int) Int -> (Int,Int) -> 
	     (Array Int a,Array Int a) -> [Operation a]
backtrack tab (0,0)  _ = []
backtrack tab (0,l1) (s1,s2)  
    = [Insert (s2 ! n) | n <- reverse [1 .. l1]]
backtrack tab (l1,0) (s1,s2)  
    = [Delete (s1 ! n) | n <- reverse [1 .. l1]]
backtrack tab (x,y) (s1,s2) = if a < b && a < c then
		        (Delete (s1 ! x)) : backtrack tab (x-1,y) (s1,s2)
                       else 
                        if b < c then
			 (Insert (s2 ! y)) : backtrack tab (x,y-1) (s1,s2)
			 else if c == d then
			         (Untouched (s1 ! x)): backtrack tab (x-1,y-1) (s1,s2)
			         else (Replace (s1 ! x) (s2 ! y)) : 
				      backtrack tab (x-1,y-1) (s1,s2)	       
 where a = tab ! (x-1,y)   -- insert
       b = tab ! (x,y-1)   -- delete
       c = tab ! (x-1,y-1) -- replace
       d = tab ! (x,y)     -- untouched

dot :: String -> [[Operation Char]] -> String
dot s xs = unlines $ ["digraph paradigm {", " rankdir=LR;", unlines (map dotify xs) ++ "}"]
 where dotify  ops   = " " ++ s ++ " -> " ++ ending ops ++ transform ops ++ " ;"
       ending ops    = [a | (Insert a ) <- ops]
       transform ops = case [(a:" -> ") ++ [b] | (Replace a b) <- ops] ++
                            [(a:" -> ")  | (Delete a ) <- ops] of
                         [] -> []
                         xs -> " [label=" ++ ("\"" ++ concat (intersperse ", " xs) ++ "\"") ++ "]"

test = putStrLn $ dot_it ["smula","smulan", "smulans", "smulas", "smulor", "smulors", "smulornas", "smulorna"]

test2 = putStrLn $ dot_it ["man","mannen", "mannens", "mans", "män", "mäns", "männen", "männens"]
