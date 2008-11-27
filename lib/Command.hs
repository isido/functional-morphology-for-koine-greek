----------------------------------------------------------------------
-- |
-- Module      : Main
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
-- > CVS $Date: 2006/06/05 15:25:00 $
-- > CVS $Author: markus $
-- > CVS $Revision: 1.1 $
--
-----------------------------------------------------------------------------
module Command where

import System.Console.GetOpt
import Maybe(fromMaybe)
import List(isSuffixOf)
import System(getProgName)
import Dict.ErrM
import IO
import UTF8
import Char
import General
import Tokenize(tokens,norm)
import Dictionary
import Frontend
import List(intersperse)
import qualified Data.Map as Map 
-- import Frontend

{- |Does the filename end with the suffix .dict? -}
is_dictionary :: FilePath -> Bool
is_dictionary = isSuffixOf ".dict"

{- |Does the filename end with the suffix .lexicon? -}
is_lexicon :: FilePath -> Bool
is_lexicon = isSuffixOf ".lexicon"

output :: [Flag] -> Maybe FilePath
output xs = case [f | Output f <- xs] of
             [f] -> Just f
             _   -> Nothing  

printer :: [Flag] -> Maybe String
printer xs = 
    case [p | (Print p) <- xs] of
     (x:_) -> return x
     _     -> Nothing


apply_encoding l flags d = case [x | Encoding x <- flags] of
                             [] -> d
                             (x:_) -> case Map.lookup x (encoding l) of
                                        Nothing -> error $ "Unknown encoding: " ++ x 
                                        Just t -> transform_dictionary t d

output_write :: [Flag] -> (String -> IO())
output_write xs = case output xs of
                    Nothing -> putStr      . encodeUTF8
                    Just f  -> writeFile f . encodeUTF8

dictionary_needed :: [Flag] -> Bool
dictionary_needed [] = True
dictionary_needed xs = not $ or [elem x nodict | x <- xs]
 where nodict = Infl : Help : Version : [Print s | s <- ["paradigms","paradigms_compact", "paradigms_latex","tagset", "core","extract","compound"]]

is_mode :: [Flag] -> Bool
is_mode xs = case [f | Mode f <- xs] of
               [_] -> True
               _   -> False  


--is_fullform :: [Flag] -> Bool
--is_fullform xs = False -- case [f | Fullform f <- xs] of
                 --  [_] -> True
                 --  _   -> False  

is_help :: [Flag] -> Bool
is_help = elem Help

is_version :: [Flag] -> Bool
is_version = elem Version

is_compound :: [Flag] -> Bool
is_compound xs = case [f | Compound f <- xs] of
                   [_] -> True
                   _   -> False  

data Comp = All | Min | Max | None | Unknown
  deriving Eq

pr_comp :: Comp -> String
pr_comp c = case c of
              All      -> "all"
              Min      -> "min"
              Max      -> "max"
              None     -> "none"
              --Length n -> "minlen=" ++ show n
              Unknown  -> "unknown (defaults to all)"

get_compound :: [Flag] -> Comp
get_compound xs = case [f | Compound f <- xs] of
                    (f@("none"):_) -> None
                    (f@("all"):_) -> All
                    (f@("min"):_) -> Min
                    (f@("max"):_) -> Max
             --       (f@('m':'i':'n':'l':'e':'n':'=':n):_) | all isDigit n -> Length (read n)
                    _     -> Unknown

is_quality :: [Flag] -> Bool
is_quality xs =  case [ x | Quality x <- xs ] of
                   [_] -> True
                   _   -> False

is_undef :: [Flag] -> Bool
is_undef = elem (Quality "undef")

is_argc :: [Flag] -> Bool
is_argc =  elem (Quality "argc")

is_unused :: [Flag] -> Bool
is_unused = elem (Quality "pop") 

is_duplicated :: [Flag] -> Bool
is_duplicated = elem (Quality "dup")

is_dict :: [Flag] -> Bool
is_dict = elem (Quality "dict")

is_all :: [Flag] -> Bool
is_all = elem (Quality "all")


is_test :: [Flag] -> Bool
is_test = elem (Quality "test") 

is_net :: [Flag] -> Bool
is_net fs = not $ null [x | Net x <- fs]

get_port :: [Flag] -> Maybe Int
get_port fs =  case [x | Net x <- fs] of
                 (x:_) | all isDigit x -> return $ read x
                 _                     -> Nothing 

get_quality :: [Flag] -> String
get_quality xs =  case [ x | Quality x <- xs ] of
                     (x:_) -> x

invalid_quality :: [Flag] -> Bool
invalid_quality xs = or [not (elem x ["undef", "pop","dup","dict","argc","all","test"]) | Quality x <- xs ] 

get_mode :: [Flag] -> String
get_mode xs = case [f | Mode f <- xs] of
               (f:_) -> f

is_printer :: [Flag] -> Bool
is_printer xs = case [f | Print f <- xs] of
                  [_] -> True
                  _   -> False  

get_length :: [Flag] -> Int
get_length xs = case [n | Length n <- xs] of
                 (n:_) | all isDigit n -> read n
                 _                     -> 0

get_number :: [Flag] -> Maybe Int
get_number fs = case [n | Number n <- fs] of
                 (n:_) | all isDigit n -> Just $ read n
                 _                     -> Nothing

get_tokenizer :: (String -> [Tok]) -> [Flag] -> (String -> [Tok])
get_tokenizer tokf fs = case get_tokenizer_name fs of
                          "words"     -> (map W . words)
                          "lines"     -> (map W . lines)
                          "norm"      -> norm . lines
                          "default"   -> tokens
                          x           -> error $ "unknown tokenizer: " ++ x

invalid_tokenizer :: [Flag] -> Bool
invalid_tokenizer fs 
 | elem (get_tokenizer_name fs) ["words","norm", "lines","default"] = False
 | otherwise                                                = True

get_tokenizer_name :: [Flag] -> String
get_tokenizer_name fs = case [t | (Tokenizer t) <- fs] of
                         (t:_) -> t
                         _     -> "default"

{-|Data type for the Command line arguments. -}
data Flag =  
       Help             |
       Synth            |
       Quality  String  | 
       Compound  String |
       Encoding  String |
       Infl             | 
       Length String    |
       DupID            |
       Tag              | 
       Number String    |
       Net String       | 
       Version          |
       Tokenizer String | 
       Mode String      |
       Print  String    |
       Output   String 
        deriving (Show,Eq)

{- |Lists all possible arguments and their explainations -}    
options :: Language l => l -> [OptDescr Flag]
options l =
    [ 
      Option ['i'] ["inflection"]  (NoArg Infl)                   "run inflection engine"
    , Option ['s'] ["synthesiser"] (NoArg Synth)                  "enter synthesizer mode"
    , Option ['a'] ["analysis"]    (NoArg Tag)                    "pos tagging"
    , Option ['c'] ["compound"]    (ReqArg Compound "COMPOUND")   "activate compound analysis (none,min,max,all)"
    , Option ['l'] ["length"]      (ReqArg Length "MINLENGTH")    "the minimum length of word forms in compounds"
    , Option ['n'] ["number"]      (ReqArg Number "MAX")          "the maximum number of analyses"
    , Option ['t'] ["tokenizer"]   (ReqArg Tokenizer "TOKENIZER") "select mode (default, words, lines, norm)"
    , Option ['m'] ["mode"]        (ReqArg Mode "MODE")           "select mode (fail, lexfail, nocomp, lexcomp)"
    , Option ['p'] ["printer"]     (ReqArg Print "PRINTER")       "print using PRINTER (core, paradigms, paradigms_compact, paradigms_latex, compound, tagset, words, lex, tables, extract, gf, xml, sfst, sfstlex, sfstheader, lexc, xfst, sql, hundict, hunaffix, lmf)"
    , Option ['e'] ["encoding"]     (ReqArg Encoding "ENCODING")   ("select another morphosyntactic encoding (" ++ (concat (intersperse ", " (Map.keys (encoding l)))) ++ ")")
    , Option ['q'] ["quality"]    (ReqArg Quality "QUALITY")       "run tests (all, test, dup, undef, pop, argc, dict)"
    --, Option ['o'] ["output"]      (ReqArg Output "FILE")           "output printer content to FILE"
 --   , Option ['g'] ["go"]        (ReqArg Net "PORTNUMBER")         "Go online with FM server on port PORTNUMBER"
    , Option ['h'] ["help"]        (NoArg Help)                     "display this message"
    , Option ['v'] ["version"]     (NoArg Version)                  "display version information"
    ]
    
{-
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"
-}

{- |Collect the valid arguments. Raises an IO error if it fails.  -} 
compilerOpts :: Language l => l -> [String] -> IO ([Flag], [String])
compilerOpts l argv = 
       case getOpt Permute (options l) argv of
        (o,xs,[] ) -> return  (o,xs)
        (_,_,errs)  -> 
	  do head <- header
 	     ioError (userError (concat errs ++ usageInfo head (options l)))

header :: IO String
header=  do prg <- getProgName
            return $ "Usage: " ++ prg ++ " [OPTION...] dictionary_file(s)..."

help :: Language l => l -> IO String
help l = do head <- header
   	    return $ usageInfo head (options l)

retrieve :: Language l => l -> [String] -> IO (Either String ([Flag],[FilePath]))
retrieve l xs = do res <- try (compilerOpts l xs)
                   case res of
                    Left io_err -> return $ Left $ ioeGetErrorString io_err
                    Right res   -> return $ Right res 
