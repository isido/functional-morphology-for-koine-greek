-- A strict general trie for types with orderings
module Trie 
    (

     Trie, 

     -- * Construction
     empty, 
     fromList, fromListWith,
     insert, insertWith,

     -- * Query
     lookup, member,

     -- * Destruction
     toList

    ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lookup)

-- The value argument couble be Maybe b, but we
-- save some memory this way
data Trie a b = Node !(Map.Map a (Trie a b))
	      | ValNode !(Map.Map a (Trie a b)) !b
-- deriving (Show)

-- | An empty trie
empty :: Trie a b
empty = Node Map.empty

-- | Construct a trie from a list of (key, value) pairs.
fromList :: Ord a => [([a], b)] -> Trie a b
fromList = fromListWith const

-- | Construct a trie from a list of (key, value) pairs
--   with a combining function for values of equals keys.
--   The order in which the elements are inserted and passed
--   to the combining function is unspecified.
fromListWith :: Ord a => (b -> b -> b) -> [([a], b)] -> Trie a b
fromListWith f = fromListWith' f id

-- | Construct a trie from a list of (key, value) pairs
--   with a combining function for values of equals keys, 
--   and a construction function used the first time a key is inserted.
--   The order in which the elements are inserted and passed
--   to the combining function is unspecified.
fromListWith' :: Ord a => (b -> c -> b) -> (c -> b) -> [([a], c)] -> Trie a b
fromListWith' f g = foldl (flip (uncurry (insertWith' f g))) empty -- foldl for strictness

-- | Insert an element into a trie.
insert ::  Ord a => [a] -> b -> Trie a b -> Trie a b
insert = insertWith const

-- | Insert an element into a trie with a combining
--   function for the value of an existing key equal to
--   the given one.
insertWith :: Ord a => (b -> b -> b) -- ^ The first argument is the existing value,
	                             --   the second is the new value.
	   -> [a] -> b -> Trie a b -> Trie a b
insertWith f = insertWith' f id


-- | Insert an element into a trie with a combining
--   function for the value of an existing key equal to
--   the given one, and a construction function used the
--   first time a key is inserted.
insertWith' :: Ord a => (b -> c -> b) -- ^ The first argument is the existing value,
	                              --   the second is the new value.
    -> (c -> b) -- ^ Used to transform the input value to a value
                --   in the map if there is no existing value.
    -> [a] -> c -> Trie a b -> Trie a b
insertWith' f g cs y = insertWith_ cs
    where 
    insertWith_ []     (Node m)      = ValNode m (g y)
    insertWith_ []     (ValNode m v) = ValNode m (f v y)
    insertWith_ (c:cs) t             = updateMap h t
	where h m = Map.insert c (insertWith_ cs t) m
		  where t = fromMaybe empty (Map.lookup c m)
    updateMap h (Node m)      = Node (h m)
    updateMap h (ValNode m v) = ValNode (h m) v

-- | Lookup a key in a trie.
lookup :: (Monad m, Ord a) => [a] -> Trie a b -> m b
lookup []     (Node _)      = fail "Not found"
lookup []     (ValNode _ v) = return v
lookup (c:cs) t             = case Map.lookup c (getMap t) of
			           Nothing -> fail "Not found"
				   Just t -> lookup cs t
    where getMap (Node m)      = m
	  getMap (ValNode m _) = m

-- | Check if a key is in a trie.
member :: Ord a => [a] -> Trie a b -> Bool
member k tr = isJust (lookup k tr)

-- | Get the (key,value) pairs from a trie.
--   The results are ordered by key.
toList :: Trie a b -> [([a],b)]
toList t = collapse t []
    where 
    collapse (Node m)      xs = rest m xs
    collapse (ValNode m v) xs = (reverse xs, v) : rest m xs
    rest m xs = concat [collapse tr (c:xs) | (c,tr) <- Map.toList m]
