{-
    Functional Morphology: Greek command definitions
    Ilja Sidoroff (ilja.sidoroff@iki.fi)

    Based on Latin command definitions by
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

module CommandsGreek where

import BuildGreek
import Frontend

commands = 
    [
     paradigm_h "d1timh"     ["τιμη"]    $ d1timh,
     paradigm_h "d1thalatta" ["θαλαττα"] $ d1thalatta,
     paradigm_h "d1hora"     ["χωρα"]    $ d1hora,
     paradigm_h "d1kriths"   ["κριτης"]  $ d1krites,
     paradigm_h "d1neanias"  ["νεανιας"] $ d1neanias,
     paradigm_h "d2logos"    ["λογος"]   $ d2logos,
     paradigm_h "d2odos"     ["ὁδος"]    $ d2odos,
     paradigm_h "d2dwron"    ["δωρον"]   $ d2dwron,
     paradigm_h "d2nous"     ["νους"]    $ d2nous,
     paradigm_h "d2ostoun"   ["οστουν"]  $ d2ostoun,
     paradigm_h "d3fylax"    ["φυλαξ"]   $ d3fylax,
     paradigm_h "d3swma"     ["σωμα"]    $ d3swma,
     paradigm_h "d3gerwn"    ["γερων"]   $ d3gerwn,
     paradigm_h "d3anhr"     ["ανηρ"]    $ d3anhr,
     paradigm_h "d3pathr"    ["πατηρ"]   $ d3pathr,
     paradigm_h "d3elpis"    ["ἐλπις"]   $ d3elpis,
     paradigm_h "d3rhtwr"    ["ρητωρ"]   $ d3rhtwr,
     paradigm_h "d3gynh"     ["γυνη"]    $ d3gynh,
     paradigm_h "w-paideuw"  ["παιδευω"] $ vwpaideuw,
     paradigm_h "w-timaw"    ["τιμαω"]   $ vwtimaw,
     paradigm_h "w-filew"    ["φιλεω"]   $ vwfilew,
     paradigm_h "w-dhlow"    ["δηλοω"]   $ vwdhlow,
     paradigm_h "prep"       ["απο"]     $ preposition,
     paradigm_h "particle"   ["και"]     $ particle,
     paradigm_h "adverb"     ["καλως"]   $ adverb,
     paradigm_h "mi-didwmi"  ["διδωμι"]  $ vmididwmi 
    ]


