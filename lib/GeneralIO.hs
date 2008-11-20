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
import General
import Dictionary
import IO
import qualified Data.Map as Map
import Frontend
import List (nub)
import Maybe(fromJust)
import Char
import ErrM
import UTF8
import qualified CTrie

type Stem      = String
type Id        = String


putStrLnUTF8 :: String -> IO()
putStrLnUTF8 s = putStrLn $ encodeUTF8 s

writeFileUTF8 f s = writeFile f (encodeUTF8 s)

writeLex :: FilePath -> Dictionary -> IO ()
writeLex f m = do h <- openFile f WriteMode
                  prFullFormLex h $ dict2fullform m

outputLex m = prFullFormLex stdout $ dict2fullform m


outputNewLex = putStrLnUTF8 . prNewDictionary 

writeTables :: FilePath -> Dictionary -> IO ()
writeTables f m = writeFileUTF8 f $ prDictionary m

outputTables m =  putStrLnUTF8 $ prDictionary m

writeGF :: FilePath -> FilePath -> Dictionary -> IO ()
writeGF f1 f2 m = writeFileUTF8 f1 $ 
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++ 
		  prGF m


outputGF f2 m = putStrLnUTF8 $ 
		"-- machine-generated GF file\n\n" ++
		"include " ++ f2 ++ " ;\n\n" ++ 
		prGF m

writeGFRes :: FilePath -> FilePath -> Dictionary -> IO ()
writeGFRes f1 f2 m = writeFileUTF8 f1 $ 
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++ 
		  prGFRes m


outputGFRes f2 m = putStrLnUTF8 $ 
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++ 
		  prGFRes m

-- writeGF1 :: FilePath -> FilePath -> Dictionary -> IO ()
-- writeGF1 f1 f2 m = writeFileUTF8 f1 $ 
-- 		   "-- machine-generated GF file\n\n" ++
--		   "include " ++ f2 ++ " ;\n\n" ++ 
--		   prGF1 m


writeXML :: FilePath -> Dictionary -> IO ()
writeXML f m = writeFileUTF8 f $ prXML m

outputXML m = putStrLnUTF8 $ prXML m

writeXFST :: FilePath -> Dictionary -> IO ()
writeXFST f m = writeFileUTF8 f $ 
		"# machine-generated XFST file\n\n" ++
		prXFST m

outputXFST m = putStrLnUTF8 $ 
	       "# machine-generated XFST file\n\n" ++
	       prXFST m

writeLEXC :: FilePath -> Dictionary -> IO ()
writeLEXC f m = writeFileUTF8 f $ 
		"! machine-generated LEXC file\n\n" ++
		prLEXC m

outputLEXC m = putStrLnUTF8 $ 
	       "! machine-generated LEXC file\n\n" ++
	       prLEXC m

writeLatex :: FilePath -> Dictionary -> IO ()
writeLatex f m = writeFileUTF8 f $ "% machine-generated LaTeX file\n" ++
		 prLatex m

outputLatex m = putStrLnUTF8 $ "% machine-generated LaTeX file\n" ++
		prLatex m

writeSQL :: FilePath -> Dictionary -> IO ()
writeSQL f m =  writeFileUTF8 f $ prSQL m

outputSQL :: Dictionary -> IO ()
outputSQL m =  putStrLnUTF8 $ prSQL m
   
analysis :: ([Attr] -> Bool) -> String -> [[String]]
analysis f s =  map (map snd) $ CTrie.decompose f s

-- trieLookup

lookupId :: String -> [String]
lookupId s = nub [identifier n | s:n:_ <-  map (words . snd) (CTrie.trie_lookup s)]
   where identifier = takeWhile (\c -> c /= ':')  . tail 

synthesiser :: Language a => a -> IO()
synthesiser l =         
    do hPutStr stdout "> "
       hFlush stdout
       s <- hGetLine stdin
       case words (decodeUTF8 s) of
	 ["q"] -> return()
	 ["c"] -> do putStrLnUTF8 $ unlines (paradigmNames l)
		     synthesiser l
         []  -> synthesiser l
         [w] -> 
           case(lookupId w) of
	    [] -> do putStrLnUTF8 $ "No entry '" ++ w ++ "' found in the lexicon." 
	             synthesiser l
	    xs   -> do putStrLnUTF8 $ "[ " ++ "<" ++ w ++ ">" 
                       sequence_ [print_table x (reverse (map snd (CTrie.trie_lookup x))) | x <- xs]
		       putStrLnUTF8 $ "]"
	   	       synthesiser l 
         x:xs -> case (parseCommand l (unwords (x:xs))) of
			    Bad s -> do putStrLnUTF8 s
			                synthesiser l
			    Ok e  -> do putStrLnUTF8 $ prDictionary $ 
					           dictionary [e]
			                synthesiser l
   where print_table id xs = do putStrLnUTF8 "{"
                                putStrLnUTF8 $ "lemma: " ++ lemma
                                putStrLnUTF8 $ "pos: " ++ pos
				putStrLnUTF8 $ "inherent(s): " ++ inhs
				putStrLnUTF8 $ "paradigm id: " ++ paradigm
				putStrLnUTF8 $ ""
                                sequence_ $ map putStrLnUTF8 $ map (takeWhile (\c -> c /= '[')) xs
                                putStrLnUTF8 "}"
          where
           (lemma:pos:_) = words $ to_space id
	   paradigm      = f id
           inhs          = to_space $ g (aux (tail (aux id))) [] -- FIXME OVERCOMPLICATED
	   to_space      = map (\c -> if c == '_' then ' ' else c)
	   aux           = snd . span (\c -> c /= '_')
	   f          [] = []
	   f ('_':'_':p) = p
	   f (x:xs)      = f xs
	   g []          zs = reverse zs
	   g ('_':'_':_) zs = reverse zs
	   g (y:ys)      zs = g ys (y:zs)

{-
synthesiser :: Language a => a -> Dictionary -> IO()
synthesiser l dict = synt 
 where table =  listToFM [((s1,s1++"_"++c),(c,xs,t,e)) | (s1,c,xs,t,e) <- (unDict dict)]
       synt =
-}


infMode :: Language a => a  -> IO()
infMode l
        = do putStr "> "
	     hFlush stdout
	     s <- getLine
	     case (words (decodeUTF8 s)) of
	      ["q"] -> putStrLnUTF8 "Session ended."
	      ["c"] -> do putStrLnUTF8 $ unlines (paradigmNames l)
			  infMode l
	      (x:xs) -> do case (parseCommand l (unwords (x:xs))) of
			    Bad s -> do putStrLnUTF8 s
			                infMode l
			    Ok e  -> do putStrLnUTF8 $ prDictionary $ dictionary [e]
			                infMode l
	      [] -> infMode l

imode :: Language a => a  -> IO()
imode l = interact (concat . map f . lines) 
  where f s =
	 case (words s) of
	  (('-':'-':_):_) -> ""
	  (x:xs) -> do case (parseCommand l (unwords (x:xs))) of
		        Bad s -> s
		        Ok e  -> unlines 
				 ["[" ++ unwords (rcw (x:xs)) ++ "]",
				  prDictionary $ dictionary [e]]
	  _     -> ""
        rcw [] = []
        rcw ("--":_) = []
        rcw (x:xs) = x:rcw xs
