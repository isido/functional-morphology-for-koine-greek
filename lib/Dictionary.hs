module Dictionary (Dict(..),
		   Dictionary,
		   Entry,
                   Paradigm,
		   prTable,
		   removeAttr,
		   FullFormLex,
		   classifyDict,
		   noAttr,
		   entry,
		   entryI,
	           entryWithInfo,
                   entryWithInfoI,
		   entryP,
		   entryIP,
	           entryWithInfoP,
                   entryWithInfoIP,
		   EntryN,
		   dictionary,
		   unDict,
		   size,
		   sizeW,
		   unionDictionary,
		   unionDictionaries,
                   emptyDict,
		   dict2fullform,
                   dict2idlex,
                   shareDictionary
		  ) where

import General
import List (intersperse,sortBy, group)
import Char
import IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as Set
import SharedString
import ErrM
import UTF8
import qualified Data.Set as Set

-- | An instance of the Dict class provides information on how
-- | to construct an entry for a given dictionary type. In 
-- | particular, it associated a word class identifier to the
-- | dictionary type.
class Param a => Dict a where
  dictword :: (a -> Str) -> String
  dictword f = concat $ take 1 $ unStr (f value0)
  category :: (a -> Str) -> String
  category = const "Undefined"
  defaultAttr :: (a -> Str) -> Attr
  defaultAttr = const noComp
  attrException :: (a -> Str) -> [(a,Attr)]
  attrException = const []
  
-- | The Dictionary ADT
data Dictionary       = Dict [Entry]

-- | Dictionary_Word = citation form
type Dictionary_Word  = String

-- | Part of speech
type Category         = String

-- | Untyped inherent
type Inherent         = String

-- | Untyped inflection parameter configuration
type Untyped          = String

-- | Untyped inflection table
type Inflection_Table = [(Untyped,(Attr,Str))]

-- | It is possible to add information about every word form,
-- | such as pronounciation.
type Extra            = [Str] 

-- | Paradigm identifier.
type Paradigm = String

-- | An Entry contains all information about a word.
type Entry = (Dictionary_Word, Paradigm, Category, [Inherent], Inflection_Table,Extra)

-- | An Entry without the compound information.
type EntryN =  (Dictionary_Word, Category, [Inherent], [(Untyped,Str)])

-- | Empty Dictionary
emptyDict :: Dictionary
emptyDict = Dict []

-- | Perform sharing on the strings in the Dictionary.
shareDictionary :: Dictionary -> Dictionary
shareDictionary (Dict xs) = Dict $ map shareEntry xs

-- | share an Entry
shareEntry :: Entry -> Entry
shareEntry (dict_word, p, cat, inhs, infl, extra) =
    (shareString dict_word, 
     shareString p, 
     shareString cat, 
     map shareString inhs, 
     shareTable infl, 
     extra) 
 where shareTable xs = map (\(a,(p,b)) -> (shareString a,(p,shareStr b))) xs

-- | create an inflection table with the compound information.
infTable :: Dict a => (a -> Str) -> Inflection_Table
infTable f = prTableAttr f (defaultAttr f) (attrException f)

-- | Translate the function encoding the extra information about the
-- | word forms to a list of strings.
extraTable ::  Dict a => (a -> Str) -> Extra
extraTable f = [s | (_,s) <- table f]
 
-- | Translate an inflection function to an Entry.
entry  :: Dict a => (a -> Str) -> Entry
entry f = entryI f [] 
  
-- | Translate an inflection function with inherent information to an Entry.
entryI :: Dict a => (a -> Str) -> [Inherent] -> Entry
entryI f ihs = (dictword f, "UP", category f, ihs, infTable f,[])

-- | Translate an inflection function with paradigm identifier to an Entry.
entryP  :: Dict a => (a -> Str) -> Paradigm -> Entry
entryP f p = entryIP f [] p
  
-- | Inflection function + inherent + paradigm identifier -> Entry
entryIP :: Dict a => (a -> Str) -> [Inherent] -> Paradigm -> Entry
entryIP f ihs p = (dictword f, p , category f, ihs, infTable f,[])

-- | Inflection function with extra information
entryWithInfo ::  Dict a => (a -> (Str,Str)) -> Entry
entryWithInfo f = entryWithInfoI f []

-- | inflection function with extra information and inherent information.
entryWithInfoI ::  Dict a => (a -> (Str,Str)) -> [Inherent] -> Entry
entryWithInfoI fun ihs = (dictword f, "UP", category f, ihs, infTable f,extraTable g)
  where f = \a -> fst (fun a)
        g = \a -> snd (fun a)
-- entryI (\a -> unionStr (f a) (unionStr (mkStr "|") (g a))) ihs

-- | inflection function with extra information and inherent information.
entryWithInfoP ::  Dict a => (a -> (Str,Str)) -> Paradigm -> Entry
entryWithInfoP f = entryWithInfoIP f [] 

-- | inflection function with extra information and inherent information
-- | and paradigm identifier. 
entryWithInfoIP ::  Dict a => (a -> (Str,Str)) -> [Inherent] -> Paradigm -> Entry
entryWithInfoIP fun ihs p = (dictword f, p, category f, ihs, infTable f,extraTable g)
  where f = \a -> fst (fun a)
        g = \a -> snd (fun a)

-- | Create a table with compound attributes.
prTableAttr :: Param a => (a -> Str) -> Attr -> [(a,Attr)] -> [(String,(Attr,Str))]
prTableAttr t da ts = 
    [(prValue a,(maybe da id (lookup a ts),s)) | (a,s) <- table t]

-- | Create a table with compound attributes when compound analysis
-- | is not used. 
prTableW :: Param a => Table a -> [(String,(Attr,Str))]
prTableW t = [ (a,(noComp,s)) | (a,s) <- prTable t]


-- | Transform typed table to untyped. 
prTable :: Param a => Table a -> Table String
prTable = map (\ (a,b) -> (prValue a, b))

unDict :: Dictionary -> [Entry]
unDict (Dict xs) = xs

-- | Number of Entry:s in Dictionary
size :: Dictionary -> Int
size = length . unDict

-- | Number of word forms.
sizeW :: Dictionary -> Int
sizeW = sum . map sizeEntry . unDict

-- | Number of word forms in Entry.
sizeEntry :: Entry -> Int
sizeEntry (_,_,_,_,t,_) = length t

-- | Create a Dictionary 
dictionary :: [Entry] -> Dictionary
dictionary = Dict

-- | Concatenate two dictionaries.
unionDictionary :: Dictionary -> Dictionary -> Dictionary
unionDictionary (Dict xs) (Dict ys) = Dict $ xs ++ ys

-- | Concatenate a list of Dictionaries.
unionDictionaries :: [Dictionary] -> Dictionary
unionDictionaries = foldr unionDictionary emptyDict

-- | Remove attributes from a dictionary.
removeAttr :: Dictionary -> [EntryN]
removeAttr = map noAttr . unDict

-- | Remove attributes from Entry. Also remove extra information. 
noAttr :: Entry ->  EntryN
noAttr (d,_,c,inh,tab,_) = (d,c,inh,[(i, s) | (i,(_,s)) <- tab])

-- | Group a dictionary into categories; reverses the entries... 
classifyDict :: Dictionary -> [(Category,[Entry])]
classifyDict = foldr addNext [] . unDict
 where
  addNext entry@(_,_,cat,_,_,_) dict = case dict of
    (c,es) : dict' | cat == c -> (c, entry:es) : dict'
    ces    : dict'            -> ces           : addNext entry dict'
    []                        -> [(cat,[entry])]

-- | A list of the word form together with the analyses and compound attributes.
type FullFormLex = [(String,[(Attr,String)])]

-- | A fullform lexicon structured around the word identifier
dict2idlex :: Dictionary -> FullFormLex
dict2idlex (Dict es) = map entry2id es

-- | Translate Entry to a word identifier and its associated word forms.
entry2id :: Entry -> (String,[(Attr,String)])
entry2id (stem, para, typ, inhs, infl,extra) = 
    (name,[(a , par ++ " : " ++ (concat (intersperse "/" (unStr str)))) | (par,(a,str)) <- infl])
 where    
    name = construct_name stem typ (concat (intersperse "_" inhs)) para 


--- | Create a fullform lexicon.
dict2fullform :: Dictionary -> FullFormLex
dict2fullform (Dict es) = f es Set.empty
  where 
    f [] _ = [] 
    f (e:es) set = 
     case entry2full e of
      (xs,name) -> seq (g (Set.member name set) name)   $ 
                         [(s,[(a,d) | (a,d) <- ys]) | (s,ys) <- xs] ++ 
			  f es (Set.insert name set)
    g True name = unsafePerformIO $ 
      hPutStrLn stderr (encodeUTF8 ("Duplicated identifer: " ++ name))
    g _    n = unsafePerformIO $ return ()


entry2full :: Entry -> ([(String,[(Attr,String)])],String)
entry2full (stem, para, typ, inhs, infl,extra) = 
  (concatMap mkForm (zip infl (extra ++ repeat nonExist)),name) where
    name = construct_name stem typ (concat (intersperse "_" inhs)) para 
    mkForm ((par,(a,str)),extra) = 
        [(s1, [(a,unwords $ stem : ('(':(name++":"++w_id) ++ ")") : typ : sp : par : (ex extra inhs))]) 
         --         LD stem (name ++":"++w_id) typ par inhs extra)]) 
         | 
               (s1,w_id,extra) <- (zip3
                                   (unStr str)
                                   (map show [1..])
                                   (unStr extra ++ (repeat [])))]
    sp = "-"
    ex []     []     = []
    ex (x:xs) []     = sp : [(x:xs)]
    ex []     (y:ys) = sp : (y:ys)
    ex (x:xs) (y:ys) = sp : (x:xs) : sp : (y:ys)


-- | Create word identifier.
construct_name :: String -> String -> String -> String -> String
construct_name stem typ inhs para = nospace $ stem++"_"++typ++ inhs' ++ "__" ++para
 where
  inhs'   = if null inhs then "" else "_" ++ inhs
  nospace = map (\c -> if (c==' ') then '_' else c)

-- | Build Haskell Trie
-- buildTrie :: Dictionary -> SATrie
-- buildTrie = tcompile . dict2fullformwithLexData  
--  where dict2fullformwithLexData (Dict es) = f es Set.empty
--       f []     _   = []
--       f (e:es) set = case entry2full e of 
--                       (xs,name) -> seq (g (Set.member name set) name)   $ 
--		                       xs ++ f es (Set.insert name set)
--       g True name = unsafePerformIO $ 
--          hPutStrLn stderr (encodeUTF8 ("Duplicated identifer: " ++ name))
--       g _    n = unsafePerformIO $ return ()


