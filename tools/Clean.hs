module Main where

import System
import List
import Char
import IO

nWords :: String -> [String]
nWords [] = []
nWords (c:cs) 
 | not (isSpace c) = case span (not . isSpace) cs of
                       (xs,ys) -> ((case c of
				 'Á' -> 'á'
				 'É' -> 'é'
				 'Í' -> 'í'
				 'Ó' -> 'ó'
				 'U' -> 'ú'--obs!
				 'Ü' -> 'ü'
				 'Ý' -> 'ý'
				 'Ñ' -> 'ñ'				
				 c   -> toLower c):xs):nWords ys
 | isSpace c    = nWords cs
 where 
  alphanumeric c = isAlpha c || elem c "ÁáÉéÍíÓóúÜüÝýÑñ"

main 
    = do xs <- getArgs
	 case xs of
	  [file] -> do prErr welcome
                       s <- readFile file
		       let ws = unlines $ map unwords $ 
				          sort $ nub  $ 
					  map nWords  $ lines s
		       putStr ws
	  _ -> do prErr welcome
                  pr <- getProgName
		  putStrLn $ "Usage: " ++ pr ++ " <lexicon file>\n"

welcome = unlines
	    [
	     "",
	     "********************************************",
	     "*  Functional Morphology Lexicon Cleaner   *",
	     "********************************************",
	     "* (c) Markus Forsberg                      *",
	     "* under GNU General Public License.        *",
	     "********************************************",
	     ""
	    ]

prErr s =  hPutStr stderr (s ++ "\n")
