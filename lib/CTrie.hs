----------------------------------------------------------------------
-- |
-- Module      : Dictionary
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
-- > CVS $Date: 2006/10/15 15:54:16 $
-- > CVS $Author: markus $
-- > CVS $Revision: 1.19 $
--
-- A Trie ADT for Functional Morphology 
-----------------------------------------------------------------------------
module CTrie (
              buildTrie, 
              buildTrieDict, 
              buildTrieDictSynt,
              buildTrieWordlist,
              trie_lookup, 
              isInTrie, 
              decompose
             ) where

import System.IO
import System.IO.Unsafe
import List
import Foreign.C
import Data.Char (isDigit)
import Monad(when)
import Dictionary(dict2fullform,dict2idlex,Dictionary,FullFormLex)

foreign import ccall "trie_lib.h build"        build     :: CString -> IO()
foreign import ccall "trie_lib.h lookup_t"     lookup_t  :: CString -> IO()
foreign import ccall "trie_lib.h start"        start     :: IO()
foreign import ccall "trie_lib.h stop"         stop      :: IO()
foreign import ccall "trie_lib.h next"         next      :: IO CString 
foreign import ccall "trie_lib.h more"         more      :: IO CInt
foreign import ccall "trie_lib.h getNumber"    getNumber :: CString -> CInt
foreign import ccall "trie_lib.h in_t"         in_t      :: CString -> CInt
foreign import ccall "trie_lib.h empty"        empty     :: IO()
foreign import ccall "trie_lib.h insert_t"     insert_t  :: CString -> CString -> IO()
foreign import ccall "trie_lib.h reversed"     reversed  :: IO()
foreign import ccall "trie_lib.h no_count"     no_count  :: IO()

type Attr = Int

-----------------------------------------------------------------------
{- | Constructs a C-trie from a file containing a fullform lexicon. -}
buildTrie :: FilePath -> Bool -> IO ()
buildTrie f b = do fc <- newCString f
		   when b reversed
		   build fc

{- | Constructs a C-trie from a Dictionary ADT. Note that the trie
   is not handled in Haskell. It is instead a global object in C. -}
buildTrieDict :: Dictionary -> Bool -> IO ()
buildTrieDict d b = do empty
		       when b reversed
                       start
                       build_it $ prLex $ dict2fullform d
		       stop

buildTrieDictSynt :: Dictionary -> Bool -> IO ()
buildTrieDictSynt d b = do empty
		           when b reversed
                           start
                           build_it $ prLex $ (dict2fullform d) 
                           no_count
                           build_it $ prLex $ (dict2idlex d)
		           stop

prLex :: FullFormLex -> [(String,String)]
prLex = concat . map prOne where
  prOne (s,ps)  = [(s,a) | a <- map prAttr ps]	
  prAttr (a,ss) = ss ++ prCompAttr a
  prCompAttr a  = " [" ++ show a ++ "] "

{- | Inserts the wordform-analysis pairs into the C-trie. -}
build_it :: [(String,String)] -> IO()
build_it     [] = return ()
build_it ((w1,w2):xs) = do c1 <- newCString w1
			   c2 <- newCString w2
			   insert_t c1 c2
			   build_it xs

{- |Build an undecorated trie (a simple trie). -}
buildTrieWordlist :: [String] -> Bool -> IO ()
buildTrieWordlist xs b = do empty
                            when b reversed
                            start
                            build_it (zip xs (repeat []))
			    stop

trie_lookup :: String -> [(Attr,String)]
trie_lookup = cstring2string . lookup_trie

lookup_trie :: String -> [(Attr,CString)]
lookup_trie s = 
            unsafePerformIO $
                do withCString s lookup_t
                   process
   where process = do i <- more
                      case i == 1 of
                       False -> return []
                       _  -> do cs <- next
                                xs <- process
                                return ((toInt (getNumber cs),cs):xs)
         toInt = fromInteger . toInteger



{- |Is the string a member in the trie? -}
isInTrie :: String -> Bool
isInTrie s = unsafePerformIO $
              do sc <- newCString s
	         return $ in_t sc == 1

{- |Compound analysis -}
decompose :: ([Attr] -> Bool) -> String -> [[(Attr,String)]]
decompose _  [] = []
decompose f sentence = 
 	concat $ map ((map cstring2string) . legal f) $ deconstruct sentence

{- |Translates the CString:s to String:s.-}
cstring2string :: [(Attr,CString)] -> [(Attr,String)]
cstring2string = map f 
  where f (a,cs) = (a,unsafePerformIO $ peekCString cs)

{- |Is the set of substrings legal with respect to the compound
   attributes? -}
legal :: ([Attr] -> Bool) -> [String] -> [[(Attr,CString)]]
legal f input = removeInvalids attrValues
 where
  removeInvalids [] = [] -- Remove all invalid analysis
  removeInvalids (xs:xss)
   | f (map fst xs)           = xs : removeInvalids xss -- Sequence valid
   | otherwise                = removeInvalids xss
  flatten       [] = [[]] -- combine all analyses with all other analyses
  flatten (xs:xss) = [x:ys | x <- xs, ys <- res] 
      where res = flatten xss
  attrValues = flatten $ map (lookup_trie) input

{- |Deconstruct a string into substrings that appears in the trie without
   considering the compound attributes. -}
deconstruct :: String -> [[String]]
deconstruct [] = [[]]
deconstruct s  = 
    concat [map (p:) (deconstruct r) | (p@(_:_),r) <- zip (inits s) (tails s),
	                                              isInTrie p]
