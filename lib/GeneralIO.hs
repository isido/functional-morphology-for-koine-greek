----------------------------------------------------------------------
-- |
-- Module      : GeneralIO
-- Maintainer  : Markus Forsberg
-- Stability   : 
-- Portability : 
--
-- Top-level functions: 
-- * reading/writing morphology databases
-- * writing Lexicon, Tables, GF, XFST, Latex 
-- * analysis/synthesis (Trie)
--
-----------------------------------------------------------------------------
module GeneralIO where

import Print
import Dict.GetDict(getEntry)
import DictToDictionary
import General
import Dictionary
import IO
import Command (Comp(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frontend
import List (nub,sort,intersperse)
import Maybe(fromJust)
import Char
import Dict.ErrM
import UTF8
import Util
import Compound
import Monad(when)
import qualified CTrie

type Stem      = String
type Id        = String

putStrLnUTF8 :: String -> IO()
putStrLnUTF8 s = putStrLn $ encodeUTF8 s

analysis :: Maybe Int -> Int -> Comp -> Maybe CompDesc -> ((String,String) -> [(String,String)]) -> String -> [[String]]
analysis m n mc f sandhi s =  get_max m $ post_filter mc $ map (map snd) $ CTrie.decompose n f' sandhi s
    where f' = if mc == None then Nothing else f
          get_max Nothing xs  = xs
          get_max (Just n) xs = take n xs

post_filter Min xs = case sort_length Set.empty xs of
                              ys@(r:_) -> let n = length r in takeWhile (\s -> length s == n) ys
                              _         -> []
post_filter Max xs = case sort_length_rev Set.empty xs of
                       ys@(r:_) -> let n = length r in takeWhile (\s -> length s == n) ys
                       _         -> []
post_filter _           xs = xs -- all, or default



--       post_filter (Length n) xs  = filter (\s -> and (map (fun n) s)) xs
-- [x | x <- xs, and $ map (fun n) x]
--       fun n x = case words x of
--                   (x:_) -> length x >= n
--                   _     -> False

lookupId :: String -> [String]
lookupId s = nub $ [identifier xs | xs <-  map snd (CTrie.trie_lookup False s)]
   where identifier ('\"':'i':'d':'\"':':':'\"':xs) = case span (/='\"') xs of
                                                        (i,_) -> i
         identifier (x:xs) = identifier xs
         identifier []     = []

synthesiser :: Language a => a -> IO String
synthesiser l =  
    do s <- getContents
       return $ unlines $ map f (lines (decodeUTF8 s))
 where f line =
        case(lookupId line) of
	 [] -> "{\"" ++ line ++ "\":\"-Unknown-\"}"
	 xs -> "{\"" ++ line ++ "\":{\n" ++ (concat  
              (concat [format x (reverse (map snd (CTrie.trie_lookup False x))) | x <- xs])) ++ "}\n"
       format :: String -> [String] -> [String]
       format s xs  =  ("\"" ++ s ++ "\":[\n") : (intersperse ",\n" xs) ++ ["\n]\n"]

imode :: Language a => a  -> IO String
imode l =  do s <- getContents
              return $ unlines $ map f (lines s)
  where f s = case getEntry s of
                Bad s -> []
                Ok  e -> 
                    case parseCommand l emptyParadigmErrors e of
		      (Right _) -> []
		      (Left e)  -> unlines  ["{" ++ (decodeUTF8 (quote s)) ++ ":[",
				             (concat (intersperse ",\n" (lines (prJSON (dictionary [e]))))) ++"\n]}"]

tester :: (PositiveTests,NegativeTests) -> [TestInput] -> IO ()
tester (pos,neg) rs = 
  do prErr $ "* Testbench (" ++ show (length pos) ++ " positive tests, " ++ show (length neg) ++ " negative tests)" 
     case (tester' pos rs [], tester' neg rs []) of
       ([],[]) -> prErr " All tests succeeded!\n"
       (xs,ys) -> do prErr $ (prFailed xs "Positive")  ++ "\n" ++ (prFailed ys "Negative")
                     prErr "\n"
 where
   prFailed [] s = " All " ++ s ++ " tests succeeded!"
   prFailed xs s = " "  ++ s ++ " tests failed... (count: " ++ (show (length xs)) ++ ")\n" ++ unlines [" " ++ r | r <- xs] 
   tester' :: [(TestInput -> Maybe String)] -> [TestInput] -> [String] -> [String]
   tester' _    [] xs = xs
   tester' test (s:xs) rs = tester' test xs (test_it test s rs)
   test_it :: [(TestInput -> Maybe String)] -> TestInput -> [String] -> [String]
   test_it [] _     rs = rs
   test_it (f:fs) s rs  = case f s of
                             Nothing       -> test_it fs s rs
                             Just message  -> test_it fs s (message:rs)

