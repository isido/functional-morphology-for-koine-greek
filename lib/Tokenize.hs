module Tokenize (tokens, isPunct, isPunctS, isNumber) where

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
    c:cs              -> case span (not . isSpec) (c:cs) of 
			  (w,rest) -> (W w) : ana rest
    []                -> []
  isSpec c = isSpace c || isPunct c || isDigit c
  -- uncap  w = map toLower w ! error! Do not work with UTF8

isPunct :: Char -> Bool
isPunct c = S.member c special

special = S.fromList ",.:;-?!\"%/()[]&={}_$#'~*<>{}"

isPunctS :: String -> Bool
isPunctS     [] = False
isPunctS  (c:_) = isPunct c

isNumber :: String -> Bool
isNumber    [] = False
isNumber  (c:_) = isDigit c
