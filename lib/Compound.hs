module Compound where

import General
import Maybe
import List(intersperse)

type CompDesc = [[CompAttr]]

data CompAttr = Star [Attr] | One [Attr]
 deriving (Eq,Show)

star :: [Attr] -> CompAttr
star = Star

attr :: [Attr] -> CompAttr
attr = One

step :: CompDesc -> Attr -> Maybe CompDesc
step xs a = case catMaybes (map st xs) of
              [] -> Nothing
              ys -> Just (concat ys)
 where st []             = Nothing
       st ((One a1):xs)  = if elem a a1 then Just [xs] else Nothing
       st ((Star a1):ys) = if elem a a1 then 
                               case st ys of 
                                 Nothing -> Just [((Star a1):ys)]
                                 Just rs -> Just (((Star a1):ys):rs)
                            else st ys

done :: CompDesc -> Bool
done xs = elem [] xs || not (null [x | x@[Star _] <- xs])

prCompDesc :: CompDesc -> String
prCompDesc cd = "compound schema\n" ++
                "---------------\n" ++ unlines [ n ++ ". " ++ l| (l,n) <- zip (map prOne cd) (map show [1..])]
 where
  prOne [] = []
  prOne (Star xs:ys) = "[" ++ concat (intersperse "|" (map show xs)) ++ "]* " ++ prOne ys
  prOne (One  xs:ys) = "[" ++ concat (intersperse "|" (map show xs)) ++ "] " ++ prOne ys
 