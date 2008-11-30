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

-----------------------------------------------
-- Interface functions for Nouns.
----------------------------------------------

d1timh :: DictForm -> Entry
d1timh w = feminine (decl1timh w) "n1a"

d1thalatta :: DictForm -> Entry
d1thalatta w = feminine (decl1thalatta w) "n1b"

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

d2dwron :: DictForm -> Entry
d2dwron w = neuter (decl2dwron w) "n2"

d2nous :: DictForm -> Entry
d2nous w = masculine (decl2nous w) "n2"

d2ostoun :: DictForm -> Entry
d2ostoun w = neuter (decl2ostoun w) "n2"

d3fylax :: DictForm -> Entry
d3fylax w = masculine (decl3fylax w) "n3a"

d3swma :: DictForm -> Entry
d3swma w = neuter (decl3swma w) "n3b"

d3gerwn :: DictForm -> Entry
d3gerwn w = masculine (decl3gerwn w) "n3c"

d3anhr :: DictForm -> Entry
d3anhr w = masculine (decl3anhr w) "n3d"

d3pathr :: DictForm -> Entry
d3pathr w = masculine (decl3pathr w) "n3e"


d3elpis :: DictForm -> Entry
d3elpis w = feminine (decl3elpis w) "n3f"

d3rhtwr :: DictForm -> Entry
d3rhtwr w = masculine (decl3rhtwr w) "n3g"

d3gynh :: DictForm -> Entry
d3gynh w = feminine (decl3gynh w) "n3h"
-----------------------------------------------

preposition :: DictForm -> Entry
preposition s = entryP (mkPreposition s) "inv"

particle :: DictForm -> Entry
particle s = entryP (mkParticle s) "inv"

adverb :: DictForm -> Entry
adverb s = entryP (mkAdverb s) "adv"

adverbIrreg :: DictForm -> DictForm -> DictForm -> Entry
adverbIrreg x y z = entryP (mkAdverbIrreg x y z) "adv"

{-
adj1durus :: DictForm -> Entry
adj1durus durus = adj1bonus durus (dur ++ "ior") (dur ++ "issimus") 
  where dur = tk 2 durus

adj1bonus :: DictForm -> DictForm -> DictForm -> Entry
adj1bonus bonus melior optimus = 
    entryP (decl1Adj bonus melior optimus decl1bonus) "adj1"

adj1tener :: DictForm -> Entry
adj1tener tener = entryP (decl1Adj tener 
		                   (tener ++ "ior") 
				   (tener ++ "imus") 
				   decl1tener) "adj1"

adj1sacer :: DictForm -> Entry
adj1sacer sacer = entryP (decl1Adj sacer 
		           (sacr ++ "ior") 
			   (sacr ++ "issimus") 
			   decl1sacer) "adj1"
 where sacr = tk 2 sacer ++ "r"
 
 -}
------------------------------------------------

vwpaideuw :: DictForm -> Entry
vwpaideuw s = entryP (vPaideuw s) "vw"

vwtimaw :: DictForm -> Entry
vwtimaw s = entryP (vTimaw s) "vaw"

vwfilew :: DictForm -> Entry
vwfilew s = entryP (vFilew s) "vew"

vwdhlow :: DictForm -> Entry
vwdhlow s = entryP (vDhlow s) "vow"

vmididwmi :: DictForm -> Entry
vmididwmi s = entryP (vDidwmi s) "vmi"

v1amare :: DictForm -> Entry
v1amare s = entryP (vAmare s) "v1"

v2habere :: DictForm -> Entry
v2habere s = entryP (vHabere s) "v2"

------------------------------------------------

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
