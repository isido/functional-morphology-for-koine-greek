{-
    Functional Morphology: Greek Dictionary definitions
    Ilja Sidoroff (ilja.sidoroff@iki.fi)

    Based on Latin Dictionary Definitions by 
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
module BuildGreek where

import RulesGreek
import AttrGreek
import TypesGreek
import Dictionary
import General


--- Interface functions for Nouns.

d1time :: DictForm -> Entry
d1time w = feminine (decl1time w) "n1a"

d1thalassa :: DictForm -> Entry
d1thalassa w = feminine (decl1thalassa w) "n1b"

d1hora :: DictForm -> Entry
d1hora w = feminine (decl1hora w) "n1c"

d1krites :: DictForm -> Entry
d1krites w = masculine (decl1krites w) "n1d"

d1neanias :: DictForm -> Entry
d1neanias w = masculine (decl1neanias w) "n1e"

d2logos :: DictForm -> Entry
d2logos w = masculine (decl2logos w) "n2"

d2odos :: DictForm -> Entry
d2odos w = feminine (decl2logos w) "n2"

d2doron :: DictForm -> Entry
d2doron w = neuter (decl2doron w) "n2"

d2nous :: DictForm -> Entry
d2nous w = masculine (decl2nous w) "n2"

d2ostoun :: DictForm -> Entry
d2ostoun w = neuter (decl2ostoun w) "n2"

d3fylax :: DictForm -> Entry
d3fylax w = masculine (decl3fylax w) "n3a"

d3soma :: DictForm -> Entry
d3soma w = neuter (decl3soma w) "n3b"

d3geron :: DictForm -> Entry
d3geron w = masculine (decl3geron w) "n3c"

d3aner :: DictForm -> Entry
d3aner w = masculine (decl3aner w) "n3d"

d3pater :: DictForm -> Entry
d3pater w = masculine (decl3pater w) "n3e"

d3elpis :: DictForm -> Entry
d3elpis w = feminine (decl3elpis w) "n3f"

d3retor :: DictForm -> Entry
d3retor w = masculine (decl3retor w) "n3g"

d3gyne :: DictForm -> Entry
d3gyne w = feminine (decl3gyne w) "n3h"

--- Adjectives

--- First & Second declension, "light"
adj12l :: DictForm -> Entry
adj12l w = entryP (decl12Adj Light w ) "adj1"

--- First & Second declension, "heavy"
adj12h :: DictForm -> Entry
adj12h w = entryP (decl12Adj Heavy w) "adj1"

--- Second declension, two-termination
adj2 :: DictForm -> Entry
adj2 w = entryP (decl2Adj w) "adj2"
                           
--- Prepositions, particles, adverbs

preposition :: DictForm -> Entry
preposition s = entryP (mkPreposition s) "inv"

particle :: DictForm -> Entry
particle s = entryP (mkParticle s) "inv"

adverb :: DictForm -> Entry
adverb s = entryP (mkAdverb s) "adv"

adverbIrreg :: DictForm -> DictForm -> DictForm -> Entry
adverbIrreg x y z = entryP (mkAdverbIrreg x y z) "adv"

--- Verbs

vpaideuo :: DictForm -> Entry
vpaideuo s = entryP (vPaideuo s) "ω"

vtimao :: DictForm -> Entry
vtimao s = entryP (vTimao s) "άω"

vfileo :: DictForm -> Entry
vfileo s = entryP (vFileo s) "έω"

vdeloo :: DictForm -> Entry
vdeloo s = entryP (vDeloo s) "όω"

vdidomi :: DictForm -> Entry
vdidomi s = entryP (vDidomi s) "μι"

vlambano :: DictForm -> DictForm -> Entry
vlambano s1 s2 = entryP (vLambano s1 s2) "ω2" 

---

noun :: Noun -> Gender -> Paradigm -> Entry
noun n g p = entryIP n [prValue g] p

feminine :: Noun -> Paradigm -> Entry
feminine n = noun n Feminine

masculine :: Noun -> Paradigm -> Entry
masculine n = noun n Masculine

neuter :: Noun -> Paradigm -> Entry
neuter n = noun n Neuter

-- Dictionary instances

instance Dict NounForm      where category _ = "Noun"
instance Dict VerbForm      where category _ = "Verb"
				  dictword f = 
				      concat $ take 1 $ unStr $ 
					   f (Infinitive PresentI Active)
instance Dict AdverbForm    where category _ = "Adverb"
instance Dict AdjectiveForm where category _ = "Adjective"
instance Dict PrepForm      where category _ = "Preposition"
instance Dict ParticleForm  where category _ = "Particle"
--    where category    _ = "Particle"
--	  defaultAttr _ = atS
