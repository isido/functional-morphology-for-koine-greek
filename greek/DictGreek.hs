{-
    Functional Morphology: Greek internal dictionary
    Ilja Sidoroff (ilja.sidoroff@iki.fi)

    Based on Latin internal dictionary
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

module DictGreek where

import BuildGreek
import Dictionary
import TypesGreek

greekDict :: Dictionary
greekDict = dictionary $ nounsAdj -- ++ particles -- ++ adverbs

nounsAdj = []
{--
nounsAdj = [
            d2logos "λογος",
            d2dwron "δωρον",
            d2nous "νους",
            d2ostoun "οστουν",
            d3fylax "φυλαξ",
            d3swma "σωμα",
            d3gerwn "γερων",
            d3anhr "ανηρ",
            d3pathr "πατηρ",
            d3elpis "ἐλπις",
            d3rhtwr "ρητωρ",
            d3gynh "γυνη"
           ]
--}
{-
         d2gladius "gladius",
	 d2gladius "filius",
	 d2gladius "socius",
	 d2bellum "donum",
	 adj1durus "durus",
	 adj1bonus "bonus" "melior" "optimus",
	 adj1bonus "magnus" "major" "maximus",
	 adj1bonus "malus" "pejor" "pessimus",
	 adj1bonus "superus" "superior" "supremus",
	 adj1tener "tener",
	 adj1tener "miser",
	 adj1sacer "sacer",
	 adj1sacer "ater" 
      ] 
-}
{--
adverbs = [
	   adverbIrreg "bene" "melius" "optime",
	   adverbIrreg "diu" "diutius" "diutissime",
	   adverbIrreg "intus" "interius" "intime",
	   adverbIrreg "mangopere" "magis" "maxime",
	   adverbIrreg "male" "peius" "pessime",
	   adverbIrreg "multum" "plus" "plurimum",
	   adverbIrreg "nequiter" "nequius" "nequissime",
	   adverbIrreg "paulum" "minus" "minime",
	   adverbIrreg "post" "posterius" "postremo"
	  ]

--}
--particles = map particle ["ne"]
--particles = map particle ["και"]