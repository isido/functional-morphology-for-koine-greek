module General where

import List (isPrefixOf,union)
import SharedString
import UTF8

-- infixr 5 +/

-- | list of word forms. 
newtype Str = Str [String]   
  deriving (Show, Eq, Ord)

-- | Inflection table
type Table  a = [(a,Str)]  

-- | Finite inflection function
type Finite a = a -> Str   

-- | The finite parameter class  
class (Eq a, Show a) => Param a where
  values  :: [a]
  value   :: Int -> a
  value0  :: a
  prValue :: a -> String
  value n = values !! n
  value0  = value 0
  prValue = show

-- | Token type
data Tok = W String                   | 
           A (String,String)          | -- dealing with ambiguous casing.
           AA (String,String, String) | -- KALLE, Kalle, kalle
	   P String                   | 
           PD String                  |
           D String
  deriving (Eq, Show)

encodeUTF8Tok = applytok encodeUTF8

decodeUTF8Tok = applytok decodeUTF8

applytok f t = case t of
                W s -> W (f s)
                A (s1,s2) -> A (f s1, f s2)
                AA (s1,s2,s3) -> AA (f s1,f s2,f s3)
                P  s          -> P (f s)
                D  s          -> D (f s)

-- | Compound forms
type Attr = Int

-- | default compound value
noComp :: Attr 
noComp = 0

-- | Promote String to Str
mkStr :: String -> Str
mkStr     [] = nonExist
mkStr (x:xs) = Str [(x:xs)]

-- | Sharing of Str
shareStr :: Str -> Str
shareStr (Str xs) = Str $ map shareString xs

-- | Translate Str to [String]
unStr :: Str -> [String]
unStr (Str xs) = xs

-- | Translate a [String] to Str
strings :: [String] -> Str
strings = Str 

-- | Apply function to 'a' and promote the resulting String to Str.
mkStr1 :: (a -> String) -> a -> Str
mkStr1 =  (mkStr .)

-- | Apply function to all variants.
mapStr :: (String -> String) -> Str -> Str
mapStr f (Str xs) = Str (map f xs)

-- | Union of two Str
unionStr :: Str -> Str -> Str
unionStr s t = strings (unStr s `union` unStr t)

-- | Prepend a string to all variants.
(+*) :: String -> Str -> Str
s +* ss = mapStr (s++) ss

-- | Mark morpheme boundary
(+/) :: String -> String -> String
s +/ t = s ++ t -- s ++ "/" ++ t

-- | Variants listed in a string 
mkStrWords :: String -> Str
mkStrWords = strings . words

-- | take all but n characters in the end of String 
tk :: Int -> String -> String
tk i s = take (max 0 (length s - i)) s

-- | drop all but n character in the end of String
dp :: Int -> String -> String
dp i s = drop (max 0 (length s - i)) s

-- | Get the n:th char from the end of String 
ch :: Int -> String -> String
ch n s = dp 1 (tk n s)

-- | Prevent the duplication: "mus" +? "s" = "mus"
(+?) :: String -> String -> String
s +? e = case (s,e) of
  (_:_, c:cs) | last s == c -> s ++ cs 
  _ -> s ++ e

-- | Choose suffix depending on last letter of stem
ifEndThen :: (Char -> Bool) -> String -> String -> String -> String
ifEndThen cond s a b = case s of
  _:_ | cond (last s) -> a
  _ -> b

-- | Conditionally drop last letter
dropEndIf :: (Char -> Bool) -> String -> String
dropEndIf cond s = ifEndThen cond s (init s) s

-- | Apply substitution table to string. 
changes :: [(String,String)] -> String -> String
changes cs s = case lookupMark s cs of
  Just (b,e) -> e ++ changes cs (drop (length b) s)
  _ -> case s of
    c:t -> c : changes cs t
    [] -> []
 where
   lookupMark _ [] = Nothing
   lookupMark st ((b,e):ms) = 
     if isPrefixOf b st then Just (b,e) else lookupMark st ms

-- | Like changes, but only apply on prefix. 
changePref ::  [(String,String)] -> String -> String
changePref cs t = case cs of
  (s,r) : rs | isPrefixOf s t -> r ++ drop (length s) t 
             | otherwise -> changePref rs t
  _ -> t

-- | Single word form exception
except :: Param a => Finite a -> [(a,String)] -> Finite a
except f es p = case (lookup p [(a,mkStr s) | (a,s) <- es]) of
		 Nothing -> f p
		 Just s  -> s

-- | Multiple word form exception
excepts :: Param a => Finite a -> [(a,Str)] -> Finite a
excepts f es p = maybe (f p) id $ lookup p es

-- | Merge two paradigm functions.
combine :: Param a => Finite a -> Finite a -> Finite a
combine f g = \a -> unionStr (f a) (g a)

-- | Missing forms exception.
missing :: Param a => Finite a -> [a] -> Finite a
missing f as = excepts f [(a,nonExist) | a <- as]

-- | Only exception (highly degenerate).
only :: Param a => Finite a -> [a] -> Finite a
only f as = missing f [a | a <- values, notElem a as]

-- | single word form variant exception
variant :: Param a => Finite a -> [(a,String)] -> Finite a
variant f es p = case lookup p [(a,s) | (a,s) <- es] of
		  Nothing -> f p
		  Just s -> strings (s:(unStr (f p)))

-- | Multiple word form variants exception.
variants :: Param a => Finite a -> [(a,Str)] -> Finite a
variants f es p = case lookup p es of
		   Nothing -> f p
		   Just ss -> unionStr ss (f p)

-- | Missing word form 
nonExist :: Str
nonExist = Str []

-- | Filter missing forms from inflection table. 
existingForms :: Table a -> Table a
existingForms = filter (not . null . unStr . snd)

-- | Translate a finite function to a table. 
table :: (Param a) => (a -> Str) -> Table a
table f = [(v, f v) | v <- values]

-- | Used to define Param instances. 
enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]

-- | fromEnum for Param
indexVal :: (Eq a, Param a) => a -> Int
indexVal a = maybe undefined id $ lookup a $ zip values [0..]

-- | Inflection table lookup
appTable :: (Param a) => Table a -> a -> Str
appTable t a = maybe undefined id $ lookup a t 

-- | Pick the first word form
firstForm :: Param a => Table a -> Str
firstForm t = appTable t value0

--- | Create function from list of values (sensitive to order).
giveValues :: (Eq a, Param a) => [b] -> (a -> b)
giveValues bs a = bs !! indexVal a

-- | Longest common prefix for [String].
longestPrefix :: [String] -> String
longestPrefix ((c:w):ws) = 
  let (cs,rs) = unzip (map (splitAt 1) ws) 
  in
  if all (==[c]) cs then c:longestPrefix (w:rs) else ""
longestPrefix _ = ""

-- | Collect all word forms into a Str.
formsInTable :: Table a -> Str
formsInTable tab = strings $ concat [unStr ss | (_,ss) <- tab]

-- | Apply function to all word forms in table.
mapInTable :: (String -> String) -> Table a -> Table a
mapInTable f t = [(a, mapStr f ss) | (a,ss) <- t]
