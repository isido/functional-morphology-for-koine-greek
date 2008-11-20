----------------------------------------------------------------------
-- |
-- Module      : Main
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
-- > CVS $Date: 2006/03/03 17:22:23 $
-- > CVS $Author: markus $
-- > CVS $Revision: 1.5 $
--
-- Main file of FM backend.
-----------------------------------------------------------------------------
module Command where

import Distribution.GetOpt
import Maybe(fromMaybe)
import List(isSuffixOf)


{- |Does the filename end with the suffix .dict? -}
is_dictionary :: FilePath -> Bool
is_dictionary = isSuffixOf ".dict"

{- |Does the filename end with the suffix .lex? -}
is_lexicon :: FilePath -> Bool
is_lexicon = isSuffixOf ".lexicon"

{- |Have an output file (-o) been given? -}
output :: [Flag] -> Maybe FilePath
output xs = case [f | Output f <- xs] of
             [f] -> Just f
             _   -> Nothing  

fullformflag :: [Flag] -> Bool
fullformflag xs = 
    case [p | p@(Print "lex") <- xs] of
     [_] -> True
     _   -> False


{-|Data type for the Command line arguments. -}
data Flag 
     = Help          | 
       Version       | 
       Input  String | 
       Output String |
       Print  String |
       Mode   String |
       Lex    String |
       Paradigms     |
       Synth         |
       Inflect       |
       InflectB      
       deriving (Show,Eq)

{- |Lists all possible arguments and their explainations -}    
options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]         (NoArg Help)             "display this message"
    , Option ['o'] ["output"]       (OptArg outp "FILE")     "output FILE"
    , Option ['p'] ["printer"]      (ReqArg Print "PRINTER") "Print using PRINTER"
    , Option ['t'] ["tables"]       (NoArg Paradigms)        "Print paradigms"
    , Option ['m'] ["mode"]         (ReqArg Mode "MODE")     "Analysis mode"
    , Option ['f'] ["fullform"]     (ReqArg Lex "FULLFORM")   "Input fullform lexicon"
    , Option ['i'] ["inflection"]   (NoArg Inflect)           "Inflection mode"
    , Option ['b'] ["batch"]       (NoArg InflectB)           "Inflection batch mode"
    , Option ['s'] ["synthesizer"] (NoArg Synth)             "Synthesizer mode"
    ]
    
inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

{- |Collect the valid arguments. Raises an IO error if it fails.  -} 
compilerOpts :: [String] -> ([Flag], [String])
compilerOpts argv = 
       case getOpt Permute options argv of
        (o,fs,[] ) -> (o,fs)
        (_,_,errs)  -> error (usageInfo [] options)
      -- where header = "Usage: fm [OPTION...] dictionary_file(s)..."
