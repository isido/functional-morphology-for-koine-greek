
module SharedString (shareString) where
-- TODO get rid of unsafePerformIO
import Data.Hashable
import Data.HashTable.IO as H
import System.IO.Unsafe (unsafePerformIO)

type HashTable k v = H.BasicHashTable k v

{-# NOINLINE stringPool #-}
stringPool :: HashTable String String
stringPool = unsafePerformIO $ H.new

{-# NOINLINE shareString #-}
shareString :: String -> String
shareString s = unsafePerformIO $ do
    mv <- H.lookup stringPool s
    case mv of
	    Just s' -> return s'
	    Nothing -> do
		       H.insert stringPool s s
		       return s
