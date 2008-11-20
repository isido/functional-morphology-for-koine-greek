module Invariant where

import General
import Dictionary

data Invariant = Invariant
  deriving (Show,Eq,Enum,Ord,Bounded)

instance Param Invariant 
    where values = enum

instance Dict Invariant
    where category = const "Invariant"

invar = "Invariant"

type Invar = (Invariant -> Str)

invarEntry :: String -> Entry
invarEntry = entry . mkInvar

mkInvar :: String -> Invar
mkInvar s = const (mkStr s)
