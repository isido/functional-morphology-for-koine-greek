module Dictionary where

import General
import Data.List (intersperse,sortBy, group)
import Data.Char
import System.IO
import Util
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as Set
import SharedString
import Dict.ErrM
import UTF8
import Data.Maybe
import qualified Data.Set as Set


-- | An instance of the Dict class provides information on how
-- | to construct an entry for a given dictionary type. In 
-- | particular, it associated a word class identifier to the
-- | dictionary type.
class Param a => Dict a where
  dictword :: (a -> Str) -> String
  dictword f = getDictWord f
  category :: (a -> Str) -> String
  category = const "Undefined"
  defaultAttr :: (a -> Str) -> Attr
  defaultAttr = const noComp
  attrException :: (a -> Str) -> [(a,Attr)]
  attrException = const []

getDictWord f = 
   case [x | (x:_) <- map (unStr . f) values] of
    (w:_) -> w
    _     -> "ET"

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

type LemmaID = String

-- | An Entry contains all information about a word.
type Entry = (LemmaID, Dictionary_Word, Paradigm, Category, [Inherent], Inflection_Table,Extra)

-- | An Entry without the compound information.
type EntryN =  (Dictionary_Word, Category, [Inherent], [(Untyped,Str)])


get_id :: Entry -> String
get_id (i,_,_,_,_,_,_) = i

is_equal_entry :: Entry -> Entry -> Bool
is_equal_entry e1 e2 = f e1 == f e2
 where f (_,d,_,c,inhs,infl,e) = (d,c,inhs,infl,e)

-- | Empty Dictionary
emptyDict :: Dictionary
emptyDict = Dict []

emptyEntry :: Entry
emptyEntry = ([],[],[],[],[],[],[])

is_empty_entry :: Entry -> Bool
is_empty_entry e = case e of 
                     (_,[],_,[],[],[],[]) -> True
                     _                    -> False

is_empty_dictionary :: Dictionary -> Bool
is_empty_dictionary (Dict xs) = and $ map is_empty_entry xs

-- | Perform sharing on the strings in the Dictionary.
shareDictionary :: Dictionary -> Dictionary
shareDictionary (Dict xs) = Dict $ map shareEntry xs

-- | set paradigm id.
set_paradigm_id :: String -> Entry -> Entry
set_paradigm_id p (i,x@(_:_),_,c,y,z,w) = (i,x,p,c,y,z,w)
set_paradigm_id _                   e = e

-- | set pos.
set_pos :: String -> Entry -> Entry
set_pos pos (i,x,p,_,y,z,w) = (i,x,p,pos,y,z,w)

set_inhs :: [String] -> Entry -> Entry
set_inhs inhs (i,x,p,pos,_,z,w) = (i,x,p,pos,inhs,z,w)

-- | set head.
set_head :: String -> Entry -> Entry
set_head h (i,_,p,x,y,z,w) = (i,h,p,x,y,z,w)

-- | set lemma id.
set_lemma_id :: String -> Entry -> Entry
set_lemma_id i (_,x,p,pos,y,z,w) = (i,x,p,pos,y,z,w)

get_lemma_id :: Entry -> String
get_lemma_id (i,x,p,pos,y,z,w) = i


replace_attr :: Attr -> Attr -> Entry -> Entry
replace_attr a b (i,x,p,pos,y,table,w) = (i,x,p,pos,y,table',w) 
 where table' = [(u,(if n == a then b else n,str)) | (u,(n,str)) <- table]

-- | share an Entry
shareEntry :: Entry -> Entry
shareEntry (i,dict_word, p, cat, inhs, infl, extra) =
    (i,
     shareString dict_word, 
     shareString p, 
     shareString cat, 
     map shareString inhs, 
     shareTable infl, 
     extra) 
 where shareTable xs = map (\(a,(p,b)) -> (shareString a,(p,shareStr b))) xs

--  [(Untyped,(Attr,Str))]
multi_words :: ([String], Entry ,[String]) -> Entry
multi_words (xs,e,ys) = e'
 where
  f  = (ps ++) . (++ ss)
  ps = unwords xs ++ (if (null xs) then "" else " ") 
  ss = (if (null ys) then "" else " ") ++ unwords ys
  e' = case e of
        (id,dict_word, p, cat, inhs, infl, extra) ->
            (id,f dict_word, p, cat, inhs, 
             [ (p,(a,mapStr f str)) | (p,(a,str)) <- infl], extra) 

first_mw :: Category -> (String -> Entry) -> (String -> Entry)
first_mw pos f s = 
 case words s of
  (x:xs) -> set_pos pos $ multi_words ([],f x,xs)
  _      -> error $ "first, invalid multiword: " ++ s

last_mw :: Category -> (String -> Entry) -> (String -> Entry)
last_mw pos f s = set_pos pos $ multi_words (xs, f l, [])
 where ws = words s
       l  = last ws
       xs = init ws

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
entryI f ihs = (entry_id f ihs [], dictword f, "", category f, ihs, infTable f,[])

entry_id :: Dict a => (a -> Str) -> [Inherent] -> Paradigm -> LemmaID
entry_id f inhs p = construct_name (dictword f) (category f) (concat (intersperse "_" inhs)) p

-- | Translate an inflection function with paradigm identifier to an Entry.
entryP  :: Dict a => (a -> Str) -> Paradigm -> Entry
entryP f p = entryIP f [] p
  
-- | Inflection function + inherent + paradigm identifier -> Entry
entryIP :: Dict a => (a -> Str) -> [Inherent] -> Paradigm -> Entry
entryIP f ihs p = (entry_id f ihs p, dictword f, p , category f, ihs, infTable f,[])

-- | Inflection function with extra information
entryWithInfo ::  Dict a => (a -> (Str,Str)) -> Entry
entryWithInfo f = entryWithInfoI f []

-- | inflection function with extra information and inherent information.
entryWithInfoI ::  Dict a => (a -> (Str,Str)) -> [Inherent] -> Entry
entryWithInfoI fun ihs = (entry_id f ihs [], dictword f, "", category f, ihs, infTable f,extraTable g)
  where f = \a -> fst (fun a)
        g = \a -> snd (fun a)
-- entryI (\a -> unionStr (f a) (unionStr (mkStr "|") (g a))) ihs

-- | inflection function with extra information and inherent information.
entryWithInfoP ::  Dict a => (a -> (Str,Str)) -> Paradigm -> Entry
entryWithInfoP f = entryWithInfoIP f [] 

-- | inflection function with extra information and inherent information
-- | and paradigm identifier. 
entryWithInfoIP ::  Dict a => (a -> (Str,Str)) -> [Inherent] -> Paradigm -> Entry
entryWithInfoIP fun ihs p = (entry_id f ihs p, dictword f, p, category f, ihs, infTable f,extraTable g)
  where f = \a -> fst (fun a)
        g = \a -> snd (fun a)

-- | Create a table with compound attributes.
prTableAttr :: Param a => (a -> Str) -> Attr -> [(a,Attr)] -> [(String,(Attr,Str))]
prTableAttr t da ts = 
    [(prValue a,(maybe da id (lookup a ts),s)) | (a,s) <- table t]

-- | Create a table with compound attributes when compound analysis
-- | is not used. 
--prTableW :: Param a => Table a -> [(String,(Attr,Str))]
--prTableW t = [ (a,(noComp,s)) | (a,s) <- prTable t]


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
sizeEntry (_,_,_,_,_,t,_) = length t

-- | Create a Dictionary 
dictionary :: [Entry] -> Dictionary
dictionary = Dict . filter (not . is_empty_entry)

is_empty :: Dictionary -> Bool
is_empty (Dict []) = True
is_empty _         = False

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
noAttr (_,d,_,c,inh,tab,_) = (d,c,inh,[(i, s) | (i,(_,s)) <- tab])

-- | Group a dictionary into categories; reverses the entries... 
classifyDict :: Dictionary -> [(Category,[Entry])]
classifyDict = foldr addNext [] . unDict
 where
  addNext entry@(_,_,_,cat,_,_,_) dict = case dict of
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
entry2id (name, stem, para, typ, inhs, infl,_) =
    (name,[(a,concat $ "{":(jword w):jk:(jhead stem):jk :(jpos typ):
                                              (jparam par):jk:(jinhs inhs):jk:(jid name):jk:(jp para):jk:(jattr (show a)) : ["}"]) 
                                                                  | (par,(a,str)) <- infl,
                                                                    w <- unStr str])

-- name = construct_name stem typ (concat (intersperse "_" inhs)) para 

testdata :: Dictionary -> [(String,String,String,String,[String],[String],String)]
testdata (Dict es) = concat $ map createTest es
 where
   createTest (id,stem,para,typ,inhs,infl,_) = [(s,stem,typ,para,words u, inhs,id) | (u,(_,str)) <- infl, s <- f (unStr str)]
   f [] = [""]
   f xs = xs

entrywords :: Entry -> (String,[String])
entrywords (_,stem, para, typ, inhs, infl,extra) = (stem, [ x | (_,(_,str)) <- infl, x <- unStr str ])

--- | Create a fullform lexicon.
dict2fullform :: Dictionary -> FullFormLex
dict2fullform (Dict es) = concat $ map entry2full es 

entry2full :: Entry -> [(String,[(Attr,String)])]
entry2full (name, stem, para, typ, inhs, infl,_) = 
  concatMap mkForm infl where
    mkForm (par,(a,str)) = 
        [(s1, [(a,concat ["{",jword s1,jk,jhead stem,jk,jpos typ,jk,jparam par,jk,jinhs inhs,jk,jid name,jk,jp para,jk,jattr (show a),"}"])])
         | s1 <- unStr str]

jword :: String -> String
jword  s1 = "\"word\":" ++ pr_v s1

jhead :: String -> String
jhead  s1 = "\"head\":" ++ pr_v s1

jpos :: String -> String
jpos   s1 = "\"pos\":" ++ pr_v s1

jparam :: String -> String
jparam s1 = "\"param\":" ++ pr_v s1

jinhs :: [String] -> String
jinhs  s1 = "\"inhs\":" ++ "[" ++ (concat (intersperse ", " (map quote s1))) ++ "]"

jid :: String -> String
jid    s1 = "\"id\":" ++ pr_v s1

jp :: String -> String
jp     s1 = "\"p\":" ++ pr_v s1

jattr :: String -> String
jattr  s1 = "\"attr\":" ++ pr_v s1

jk :: String
jk = ","

pr_v :: String -> String
pr_v [] = show "*"
pr_v  v = quote v

--json_list :: String -> [String] -> String
--json_list field values = quote field ++ ":[" ++ (concat (intersperse ", " (map quote values))) ++ "]"

-- | Create word identifier.
construct_name :: String -> String -> String -> String -> String
construct_name stem typ inhs para = 
  case para of
   []   -> nospace $ stem++"_"++typ++ inhs'
   para -> nospace $ stem++"_"++typ++ inhs' ++ "_" ++para
 where
  inhs'   = if null inhs then "" else "_" ++ inhs
  nospace = map (\c -> if (c==' ') then '_' else c)

type Unknowns       = Set.Set String
type WrongArguments = Set.Set (String,Int)
type ParadigmErrors = (Unknowns, WrongArguments)

emptyParadigmErrors :: ParadigmErrors
emptyParadigmErrors = (Set.empty,Set.empty)

insertParadigmError :: Either String (String,Int) -> ParadigmErrors -> ParadigmErrors
insertParadigmError (Left p) (unknowns, wa) = (Set.insert p unknowns, wa)
insertParadigmError (Right a) (us, wrong_arguments) = 
    (us, Set.insert a wrong_arguments)

prErrorTable :: [String] -> String
prErrorTable ts = unlines 
              [unwords [" ",pad s1 c1,pad s2 c2, pad s3 c3] | (s1,s2,s3) <- sp]
 where f1 (x,_,_) = length x
       f2 (_,x,_) = length x
       f3 (_,_,x) = length x
       c1 = maximum (map f1 sp)
       c2 = maximum (map f2 sp)
       c3 = maximum (map f3 sp)
       sp = splits ts
       pad s c = s ++ take (c - length s) (repeat ' ')
       splits []    = []
       splits [x]   = [(x,[],[])]
       splits [x,y] = [(x,y,[])]
       splits xs = case splitAt 3 xs of
                     ([x,y,z],rs) -> (x,y,z):splits rs


equal_entry :: [(String,String)] -> Entry -> Entry -> Maybe (String,String)
equal_entry tab e1 e2
  | is_equal_entry e1 e2 = if (except tab e1 e2) then Nothing else Just (pr e1, pr e2)
  | otherwise            = Nothing
 where pr (name, stem, para, typ, inhs, infl,extra) = " {" ++ para ++":" ++ stem ++ "(" ++name ++ ")} "
       except xs  (n1, _, _, _,_,_,_) (n2, _, _, _,_,_,_) = elem (n1,n2) xs || elem (n2,n1) xs

check_duplication :: Dictionary -> [(String,String)] -> [(String,String)]
check_duplication (Dict es) tab =  catMaybes $ run_check es
  where run_check     [] = []
        run_check (e:es) = map (equal_entry tab e) es ++ run_check es

print_duplicates :: Dictionary -> [(String,String)] -> IO ()
print_duplicates d tab = case [prErr ("  " ++ (x ++ "~" ++ y)) | (x,y) <- check_duplication d tab] of
                           (x:xs) -> do prErr $ "* Identical entries detected (count: " ++ (show (length (x:xs))) ++ ")\n"
                                        sequence_ (x:xs)
                           _      -> prErr "* No identical entries detected!"

transform_dictionary (pos,inhs,param) (Dict xs) = Dict (map tr xs)
  where tr (name, stem, para, typ, inhs, infl,extra) = 
         let infl' = [(param_f a,b) | (a,b) <- infl] in
            (name,stem,para,pos_f typ, inhs_f inhs,infl',extra)
        pos_f   = f pos
        inhs_f  = f inhs
        param_f = f param
        f Nothing = id
        f (Just g) = g


map_wordforms :: (String -> String) -> Entry -> Entry
map_wordforms f (name, stem, para, typ, inhs, infl,extra) = 
     (name,stem,para,typ,inhs,infl',extra)
 where infl' = [(u,(a,strings (map f (unStr ss)))) | (u,(a,ss)) <- infl]

duplicated_lemma_id :: Dictionary -> [String]
duplicated_lemma_id (Dict es) = check (map get_lemma_id es) (Set.empty,Set.empty)
  where check [] (_,b) = Set.toList b
        check (x:xs) (s,b) 
          | Set.member x s = check xs (s,Set.insert x b)
          | otherwise      = check xs (Set.insert x s, b)
