module Frontend where

import qualified Data.Map as Map
import Dictionary
import ErrM
import Char
import General
import Maybe(isJust)
import IO
import Tokenize
import UTF8
import qualified CTrie


-- Note that all Functions have default definitions, but 
-- in the common case, you give, at least, definitions for "paradigms"
-- "internDict" and "composition"

--class Show a => Language a b | a -> b where
-- | A class defined to be able to construct a language independent frontend
class Show a => Language a  where
  name        :: a -> String  
  dbaseName   :: a -> String  
  composition :: a -> ([General.Attr] -> Bool) 
  env         :: a -> String 
  paradigms   :: a -> Commands
  internDict  :: a -> Dictionary 
  tokenizer   :: a -> String -> [General.Tok]
  wordGuesser :: a -> String -> [String]
  name        l = map toLower (show l)
  dbaseName   l = name l ++ ".lexicon"
  composition _ = noComp
    where noComp [_] = True
	  noComp   _ = False
  env         l = "FM_" ++ map toUpper (show l)
  paradigms   _ = emptyC
  internDict  _ = emptyDict  
  tokenizer   _ = tokens
  wordGuesser _ = const []

-- | type for Command Map  
type Commands = Map.Map String ([String], [String] -> Entry) 

-- | empty Command Map
emptyC :: Commands
emptyC = Map.empty

-- | add a command
insertCommand :: (String,[String],[String] -> Entry) -> Commands -> Commands
insertCommand (n,args,f) cs = Map.insert n (args,f) cs

-- | Construct a Command Map
mkCommands :: [(String,[String],[String] -> Entry)] -> Commands
mkCommands = foldr insertCommand Map.empty

-- | Create a dictionary from the list of paradigms.
command_paradigms :: Language a => a -> Dictionary
command_paradigms l = dictionary [f xs | (_,(xs,f)) <- Map.toList (paradigms l)]

-- | Parse commands.
parseCommand :: Language a => a -> String -> Err Entry
parseCommand l s = 
   case words (remove_comment s) of
    (x:xs) -> case Map.lookup x (paradigms l) of
               Nothing -> Bad $ "Undefined paradigm identifier '" ++ x ++ "'."
               Just (ys,f) -> if (length xs == length ys) then
	                         Ok $ f xs
                               else				 
                                 Bad $ "Paradigm '" ++ s ++ "' requires " ++ (show (length ys)) ++ " arguments." 
    [] -> Bad $ "No command."

-- | List paradigm names
paradigmNames :: Language a => a -> [String]
paradigmNames l = [ c ++ " " ++ unwords args | (c,(args,_)) <- Map.toList (paradigms l)]

-- | Number of paradigms.
paradigmCount :: Language a => a -> Int
paradigmCount l = length $ Map.toList (paradigms l)

-- | Reading external lexicon. Create empty lexicon if the file does not exist.
parseDict :: Language a => a -> FilePath -> IO (Dictionary,Int)
parseDict l f = 
    do (es,n) <- catch (readdict l f) (\_ -> do writeFile f [] ; prErr ("Created new external dictionary: \"" ++ f ++ "\".\n"); return ([],0))
       return $ (dictionary es,n)       

-- | Is input string a paradigm identifier?
isParadigm :: Language a => a -> String -> Bool
isParadigm l s = isJust $ Map.lookup s (paradigms l)

-- | Read external lexicon.
readdict :: Language a => a -> FilePath -> IO ([Entry],Int)
readdict l f = do h <- openFile f ReadMode
		  process l h ([],0)
 where
  process :: Language a => a -> Handle -> ([Entry],Int) -> IO ([Entry],Int)
  process l h xs =
    hIsEOF h >>= \b ->
        if b then return xs
           else
            do s <- hGetLine h
	       res <- collect (decodeUTF8 s) xs
               process l h res
  collect []     res = return res
  collect xs@(c:s) (pre,n)
   | isComment xs = return (pre,n)
   | otherwise   = case parseCommand l (remove_comment xs) of
                    Ok e  -> return (e:pre,n+1)
		    Bad s -> do prErr s
                                return (pre,n)
  isComment           [] = False
  isComment     (' ':xs) = isComment xs
  isComment ('-':'-':xs) = True
  isComment            _ = False       

-- | Remove comments in String.  
remove_comment :: String -> String
remove_comment [] = []
remove_comment ('-':'-':_) = []
remove_comment (x:xs)      = x:remove_comment xs

-- Application of lists to functions

app1 :: (String -> Entry) -> [String] -> Entry
app1 f [x] = f x
app1 _ _ = error $ "app1: wrong number of arguments"

app2 :: (String -> String -> Entry) -> [String] -> Entry
app2 f [x,y] = f x y
app2 _ _ = error $ "app2: wrong number of arguments"

app3 :: (String -> String -> String -> Entry) -> [String] -> Entry
app3 f [x,y,z] = f x y z
app3 _ _ = error $ "app3: wrong number of arguments"

app4 :: (String -> String -> String -> String -> Entry) -> [String] -> Entry
app4 f [x,y,z,w] = f x y z w
app4 _ _ = error $ "app4: wrong number of arguments"

app5 :: (String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app5 f [x,y,z,w,a] = f x y z w a
app5 _ _ = error $ "app5: wrong number of arguments"

app6 :: (String -> String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app6 f [x,y,z,w,a,b] = f x y z w a b
app6 _ _ = error $ "app6: wrong number of arguments"

app7 :: (String -> String -> String -> String -> String -> String -> String -> Entry) -> [String] -> Entry
app7 f [x,y,z,w,a,b,c] = f x y z w a b c
app7 _ _ = error $ "app7: wrong number of arguments"

-- | Print to stderr.
prErr :: String -> IO()
prErr s =  hPutStr stderr (s ++ "\n")
