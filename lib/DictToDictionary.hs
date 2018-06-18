module DictToDictionary where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Dict.Abs as Abs
import Dict.GetDict
import Dictionary
import Frontend
import System.Exit
import System.IO
import Control.Exception
import Control.Monad(when)
import System.IO -- TODO: check
import Dict.ErrM
import Util

-- | Parse commands.
parseCommand :: Language a => a -> ParadigmErrors -> Abs.Entry -> (Either Entry ParadigmErrors)
parseCommand l pe e =
  let  (i,args,t) = f e
       xs = [s | Abs.NStr s <- args] in 
   case Map.lookup i (paradigms l) of
     Nothing -> (Right (insertParadigmError (Left i) pe))
     Just (ys,f) -> let n = length xs in
                      if (n == length ys) then
	                  (Left (g t (f xs)))
                       else
			  (Right (insertParadigmError  (Right (i,n)) pe))
 where
   f (Abs.E (Abs.Ident i) args)    = (i,args,Nothing)
   f (Abs.EA (Abs.Ident i) args t) = (i,args,Just t)
   g Nothing  e = e
   g (Just t) e = termParser l t e

-- | Reading external lexicon. Create empty lexicon if the file does not exist.
parseDict :: Language a => a -> FilePath -> (Bool,Bool,Bool) -> IO (Err (Dictionary,Int))
parseDict l f  (undefcheck,argccheck,unusedcheck) = 
    do res <- catch (readdict l f (undefcheck,argccheck,unusedcheck)) 
               (\e -> do let err = show (e :: IOException)
                         prErr ("Unable to load dictionary file: \"" ++ f ++ "\": " ++ err ++ "\n")
                         exitFailure)
       case res of
         Ok (es,n) -> return $ Ok (dictionary es,n)       
         Bad s     -> return $ Bad s 

getNames                              [] = []
getNames ((Abs.E  (Abs.Ident i) _):xs)   = i:getNames xs
getNames ((Abs.EA (Abs.Ident i) _ _):xs) = i:getNames xs

-- | Read external lexicon.
readdict :: Language a => a -> FilePath -> (Bool,Bool,Bool) -> IO (Err ([Entry],Int))
readdict l f  (undefcheck,argccheck,unusedcheck) -- output_errors 
        = do s <- readFile f
             case getDict s of
               Ok (Abs.Dict es) -> 
                   do  when unusedcheck $ prError $
                        case (Set.toList (Set.difference (Set.fromList (paradigmID l)) (Set.fromList (getNames es)))) of
                         [] -> "\n* No unpopulated paradigms detected!\n" 
                         ys -> "\n* Unpopulated paradigms detected:\n" ++ prErrorTable ys
                       process l es emptyParadigmErrors ([],0)
               Bad s            -> return (Bad s)
 where
  process l [] bad_para xs = 
      do printErrors bad_para (undefcheck,argccheck)
         return (Ok xs)
  process l (e:es) bad_para xs =
	       do (bp,res) <- collect e bad_para xs
                  process l es bp res
  collect e bad_para res@(pre,n)
     = do case parseCommand l bad_para e of
            Left e   -> return (bad_para,(e:pre,n+1))
	    Right bp -> return (bp,res)

