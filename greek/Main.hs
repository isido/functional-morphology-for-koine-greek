{-
    Functional Morphology: Greek composite definition
    Ilja Sidoroff (ilja.sidoroff@iki.fi)

    Based on Latin composite definition by

    Copyright (C) 2004  Author: Markus Forsberg

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main where

import CommonMain
import CommandsGreek
import CompositeGreek
import DictGreek
import Frontend
import Guessing

main :: IO ()
main = commonMain Greek

data Greek = Greek
 deriving Show

instance Language Greek where
 internDict   _ = greekDict
 composition  _ = greek_compound
 paradigms    _ = foldr insertCommand emptyC commands
 wordGuesser  _ = silly_guesser
