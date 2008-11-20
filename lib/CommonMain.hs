module CommonMain where

import Print
import List(intersperse)
import System(getArgs, getEnv)
import GeneralIO
import General
import IO
import Dictionary
import Frontend
import Char
import ErrM
import Monad
import UTF8
-- import Tokenize
import qualified CTrie

data AnaType = 
    Normal            |
    NoAnalysis        |
    FilterLexiconNo   |
    FilterLexiconComp
    
gfTypes :: Language a => a -> String
gfTypes l = "types." ++ name l ++ ".gf"

readDict :: Language a => a -> FilePath -> IO Dictionary
readDict l f = 
    do (database,n) <- parseDict l f
       let isz = size (internDict l)
           sz  = n + isz
       prErr
          $ "Morphology Statistics:\n" ++ 
            print_lang l ++
            print_paradigms l ++ print_size (sz,n,isz)
       return $ unionDictionary (internDict l) database

print_lang l = " # language id: " ++ (name l) ++ "\n"

print_paradigms l = case paradigmCount l of
                     0 -> " # no paradigms\n # "
                     1 -> " # 1 paradigm\n # "
                     n -> " # " ++ show n ++ " paradigms\n # "

print_size (0,n,isz)  = "empty dictionary"
print_size (1,_,_)    = "1 entry"
print_size (sz,n,isz) = show nsz ++ "k entries (e: " ++ show n ++ ", i: " ++ show isz ++ ")"
  where 
        nsz = fromInteger $ round (((fromInteger (toInteger sz))::Double) / 1000)

uName :: Language a => a -> String
uName l = case name l of
	   [] -> []
	   (x:xs) -> toUpper x : xs

commonMain :: Language a => a -> IO ()
commonMain l = do
  xx <- getArgs
  
  lex <- catch (getEnv (env l)) (\_ -> 
   do prErr $ "\n[" ++ (env l) ++ " is undefined, using \"./" ++ (dbaseName l) ++ "\".]\n"
      return $ "./" ++ (dbaseName l))
  case xx of
    []             -> do prErr $ welcome l
			 d <- readDict l lex
			 CTrie.buildTrieDict d False
			 run l (analysis (composition l)) []
    ["-pos"]       -> do prErr $ welcome l
			 d <- readDict l lex
			 CTrie.buildTrieDict d False
			 posify l (analysis (composition l))
    ["-mode", an]  -> do prErr $ welcome l
		      	 d <- readDict l lex
			 CTrie.buildTrieDict d False
			 run l (analysis (composition l)) an
    ["-f",file]    -> do prErr $ welcome l
                         prErr $ "Morphology Statistics:"
			 CTrie.buildTrie file False
			 run l (analysis (composition l)) []
    ["-h"]         -> help
    ["-p"]         -> do outputTables (command_paradigms l)
    ["-s"]         -> do prErr $ welcome l
	                 putStrLn $ "\nSynthesiser mode\n"
			 putStrLn $ "Enter a " ++ (uName l) ++ " word in any form\n"
			 putStrLn $ "or a [paradigm name] with [word forms].\n"
			 putStrLn $ "Type 'c' to list paradigms.\n"
			 putStrLn $ "Type 'q' to quit.\n"
			 theDictionary <- readDict l lex
			 CTrie.buildTrieDictSynt theDictionary False
			 synthesiser l
    ["-i"]         -> do prErr $ welcome l
	                 putStrLn $ "\n[Inflection mode]\n"
			 putStrLn $ "Enter [paradigm name] with [word forms].\n"
			 putStrLn $ "Type 'c' to list paradigms.\n"
			 putStrLn $ "Type 'q' to quit.\n"
			 infMode l
    ["-ib"]         -> do prErr $ welcome l
		          imode l
    _  -> 
      do theDictionary <- readDict l lex
         case xx of
          ["-newlex"]         -> outputNewLex theDictionary
	  ["-lex"]            -> outputLex theDictionary
	  ["-lex",file]  -> do writeLex file theDictionary
	                       prErr $ "Wrote full form lexicon: " ++ file
	  ["-tables"]         -> outputTables theDictionary
	  ["-tables",file]    -> do writeTables file theDictionary
	                            prErr $ "Wrote tables: " ++ file
	  ["-gf"]             -> outputGF (gfTypes l) theDictionary
	  ["-gf",file]        -> do writeGF file (gfTypes l) theDictionary
	                            prErr $ "Wrote GF source code: " ++ file
	  ["-gfr"]            -> outputGFRes (gfTypes l) theDictionary
	  ["-gfr",file]       -> do writeGFRes file (gfTypes l) theDictionary
	                            prErr $ "Wrote GF resource: " ++ file
	  ["-latex"]          -> outputLatex theDictionary
	  ["-latex",file]     -> do writeLatex file theDictionary
	                            prErr $ "Wrote LaTeX document: " ++ file
	  ["-xml"]            -> outputXML theDictionary
	  ["-xml",file]       -> do writeXML file theDictionary
	                            prErr $ "Wrote XML source code: " ++ file
	  ["-lexc"]           -> outputLEXC theDictionary
	  ["-lexc",file]      -> do writeLEXC file theDictionary
	                            prErr $ "Wrote LEXC source code: " ++ file
	  ["-xfst"]           -> outputXFST theDictionary
	  ["-xfst",file]      -> do writeXFST file theDictionary
			            prErr $ "Wrote XFST source code: " ++ file
	  ["-sql"]            -> outputSQL theDictionary
	  ["-sql",file]   -> do writeSQL file theDictionary
			        prErr $ "Wrote SQL source code: " ++ file
	  xs             -> do prErr $ "Invalid parameter" ++ unwords xs
	                       help

data Stats = Stats {
		    totalWords :: Int,
		    coveredWords :: Int
		   }

initStats :: Stats
initStats = Stats { totalWords = 0, coveredWords = 0 }

posify :: Language a => a -> (String -> [[String]]) -> IO ()
posify l f = do 
  s' <- hGetContents stdin
  let ts  = tokenizer l (decodeUTF8 s')
      tss = get_sentences ts
  sequence_ [printResult (map anapos x) | x <- tss]
 where 
  printResult [] = return ()
  printResult xs = do hPutStrUTF8 stdout $ "{"
                      sequence_ xs
                      hPutStrLnUTF8 stdout $ "}"
  anapos t = 
   case t of
    (P s) -> hPutStrUTF8 stdout $ "(\"" ++ esc s ++ "\",spec)"
    (D s) -> hPutStrUTF8 stdout $ "(\"" ++ esc s ++ "\",num)"
    (W s) ->  
	case f s of
        [] -> hPutStrUTF8 stdout $ "(\"" ++ esc s ++ "\",)"
        xs -> hPutStrUTF8 stdout $ "(\"" ++ esc s ++ "\"," ++ prResult xs ++ ")"
  prResult        xs  = concat $ intersperse "|" $
                        filter (not . null) (map filter_analysis xs)
  filter_analysis [s] = unwords $ clean (drop 2 (words s))
  filter_analysis _   = ""
  esc   [] = []
  esc   ('\\':xs) = '\\':'\\':esc xs
  esc   ('"':xs) = '\\':'"':esc xs
  esc   (c:cs)   = c:esc cs
  clean []           = []
  clean ("-":xs)     = clean xs
  clean (('[':_):xs) = clean xs
  clean (x:xs)       = x:clean xs

get_sentences :: [Tok] -> [[Tok]]
get_sentences xs = gets xs []
   where gets [] s = [reverse s]
         gets (c:cs) s
          | isMajor c = (reverse (c:s)):gets cs []
          | otherwise = gets cs (c:s)

isMajor (P [c]) = elem c ".?!"
isMajor _     = False

run :: Language a => a -> (String -> [[String]]) -> String -> IO ()
run l f "fail"      = run' l f NoAnalysis        initStats >> return ()
run l f "lexfail"   = run' l f FilterLexiconNo   initStats >> return ()
run l f "lexcomp"   = run' l f FilterLexiconComp initStats >> return ()
run l f _           = do
		      st <- run' l f Normal            initStats
		      prErr $ "Total words:   " ++ show (totalWords st)
		      prErr $ "Covered words: " ++ show (coveredWords st)

run' :: Language a => a -> (String -> [[String]]) -> AnaType -> Stats -> IO Stats
run' l f t st = 
 do 
   b <- hIsEOF stdin
   if b then return st 
    else do 
          s' <- hGetLine stdin
          let s = decodeUTF8 s'
	  case t of
	    FilterLexiconNo   -> 
             analyze l t f (map W (words s)) st >>= run' l f t
	    FilterLexiconComp -> 
	     analyze l t f (map W (words s)) st >>= run' l f t
	    _                 ->  
	     analyze l t f (tokenizer l s) st >>= run' l f t

word_tokens :: [Tok] -> [String]
word_tokens xs = [s | (W s) <- xs]

hPutStrLnUTF8 h s = hPutStrLn h $ encodeUTF8 s

hPutStrUTF8   h s = hPutStr h   $ encodeUTF8 s

analyze :: Language a => a -> AnaType -> (String -> [[String]]) -> [Tok] -> Stats -> IO Stats
analyze _ FilterLexiconNo f ((W x):xs) st
     | or $ map (\ys -> length ys > 0) (concat (map f (word_tokens xs))) = 
	 do
	 hPutStrLnUTF8 stdout $ unwords (x:(word_tokens xs))
	 return st
     | otherwise = return st
analyze _ FilterLexiconComp f ((W x):xs) st
     | or $ map (\ys -> length ys > 1) (concat (map f (word_tokens xs))) = 
	 do
	 hPutStrLnUTF8 stdout $ unwords (x:(word_tokens xs))
	 return st
     | otherwise = return st
analyze _ _ _  [] st = return st
analyze l t f (s:ss) st =
   case s of
    (P s) ->   
        do case t of 
	    Normal    
             -> do hPutStrLnUTF8 stdout $ "[ <" ++ s ++ "> SPECIAL]"
		   analyze l t f ss st
            _ -> analyze l t f ss st
    (D s) ->
        do case t of 
            Normal 
             -> do hPutStrLnUTF8 stdout $ "[ <" ++ s ++ "> NUMBER]"
		   analyze l t f ss st
	    _ -> analyze l t f ss st
    (W s) ->  
	case f s of
        [] -> do prAnalysis l t s []
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st 
			            })
        xs -> do prAnalysis l t s xs
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st + 1 
			            })


prAnalysis :: Language a => a -> AnaType -> String -> [[String]] -> IO()
prAnalysis l Normal   s   [] = case wordGuesser l s of
                                 (x:xs) ->  do mapM_ (hPutStrUTF8 stdout) ["[ <", s, "> GUESS\n"]
                                               prA (map (:[]) (x:xs))
                                               hPutStrLnUTF8 stdout "]"
                                 []     ->
                                   mapM_ (hPutStrUTF8 stdout) ["[ <", s, "> NONE]\n"]
prAnalysis _ Normal   s   xs = do mapM_ (hPutStrUTF8 stdout) ["[ <", s, ">\n"]
                                  prA xs
                                  hPutStrLnUTF8 stdout "]"
prAnalysis _ NoAnalysis s []        = hPutStrLnUTF8 stdout $ s
prAnalysis _ NoAnalysis s _         = return ()
prAnalysis _ _      _ _             = return ()

prA xs =  mapM_ (hPutStrLnUTF8 stdout)
                 [n ++ ('.':' ':s) | (n,s) <- zip (map show [1..]) (map pr xs)]

pr []  = []
pr [x] = x
pr xs  = "Composite: " ++ concat (intersperse " | " xs)


--prAnalysis FilterLexiconComp s xs = if (or $ map (\xs -> length xs > 1)) then
--                                    hPutStrLnUTF8 stdout $ s
--                                   else return ()

welcome :: Language a => a -> String
welcome l = unlines
	    [
             "",
	     "*************************************",
	     "*   Functional Morphology v2.0      *",
	     "* (c) M. Forsberg & A. Ranta 2007   *",
	     "* under GNU General Public License. *",
	     "*************************************"
	    ]
  where padding s n = replicate (max (n - length s) 0) ' '
 
help :: IO()
help = prErr help_text

help_text :: String
help_text = unlines ["",
		     " |---------------------------------------|",
		     " |        Program parameters             |",
		     " |---------------------------------------|",
		     " | -h             | Display this message |",
		     " |---------------------------------------|",
		     " | <None>         | Enter tagger mode    |",
		     " |---------------------------------------|",
		     " | -s             | Enter interactive    |",
		     " |                | synthesiser mode     |",
		     " |---------------------------------------|",
		     " | -i             | Enter inflection     |",
		     " |                | mode                 |",
		     " |---------------------------------------|",
		     " | -ib            | Inflection batch     |",
		     " |                | mode                 |",
		     " |---------------------------------------|",
		     " | -lex    [file] | Full form lexicon    |",
		     " | -tables [file] | Tables               |",
		     " | -gf     [file] | GF top-level code    |",
		     " | -gfr    [file] | GF resource code     |",
		     " | -latex  [file] | LaTeX source code    |",
		     " | -xml    [file] | XML source code      |",
		     " | -lexc   [file] | LexC source code     |",
		     " | -xfst   [file] | XFST source code     |",
		     " | -sql    [file] | SQL source code      |",
	       	     " |---------------------------------------|",
		     ""
		    ]

