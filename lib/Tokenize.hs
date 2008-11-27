module Tokenize (tokens, norm, isPunct, isPunctS, isNumber) where

import Char
import List
import System
import General
import qualified Data.Set as S

tokens :: String -> [General.Tok]
tokens = ana where
  ana [] = []
  ana s = case s of
    c:cs  | isSpace c -> ana cs
    c:cs  | isDigit c -> case span (isDigit) (c:cs) of 
			  (w,rest) -> (D w) : ana rest
    p:cs  | isPunct p -> (P [p]) : ana cs  
    c:cs              -> case span (\c -> (not (isSpec c)) || (isDash c)) (c:cs) of 
			  (w,rest) -> (uncap w) : ana rest
    []                -> []
  isSpec c = isSpace c || isPunct c || isDigit c
  isDash c = c == '-'

uncap  w -- = W $ map toLower w
   | and (map isLower w) = W w
   | sum (map (f.isUpper) w) > 1 = case map toLower w of
                                    (c:cs) -> AA (w,(toUpper c:cs),(c:cs))
   | otherwise           = A (w, map toLower w)
 where
  f False = 0
  f True  = 1

isPunct :: Char -> Bool
isPunct c = S.member c special

norm :: [String] -> [Tok]
norm = map f
 where
  f w@(c:cs) 
   | isUpper c = A (w, map toLower w)
   | all isDigit w = D w
   | all isPunct w = P w
   | all (\c -> isDigit c || isPunct c || isSpace c) w = PD w
   | otherwise = W (c:cs)

-- - removed from special.
special = S.fromList "°§+»,.:;-?!\"%/()[]&={}_$#'~*<>{}"

isPunctS :: String -> Bool
isPunctS     [] = False
isPunctS  (c:_) = isPunct c

isNumber :: String -> Bool
isNumber    [] = False
isNumber  (c:_) = isDigit c
