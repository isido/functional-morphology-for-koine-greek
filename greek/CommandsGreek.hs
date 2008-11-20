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
     ("d1timh", ["τιμη"], app1 d1timh),
     ("d1thalatta", ["θαλαττα"], app1 d1thalatta),
     ("d1hora", ["χωρα"], app1 d1hora),
     ("d1kriths", ["κριτης"], app1 d1krites),
     ("d1neanias", ["νεανιας"], app1 d1neanias),
     ("d2logos", ["λογος"], app1 d2logos),
     ("d2odos", ["οδος"], app1 d2odos),
     ("d2dwron", ["δωρον"], app1 d2dwron),
     ("d2nous", ["νους"], app1 d2nous),
     ("d2ostoun", ["οστουν"], app1 d2ostoun),
     ("d3fylax", ["φυλαξ"], app1 d3fylax),
     ("d3swma", ["σωμα"], app1 d3swma),
     ("d3gerwn", ["γερων"], app1 d3gerwn),
     ("d3anhr", ["ανηρ"], app1 d3anhr),
     ("d3pathr", ["πατηρ"], app1 d3pathr),
     ("d3elpis", ["ἐλπις"], app1 d3elpis),
     ("d3rhtwr", ["ρητωρ"], app1 d3rhtwr),
     ("d3gynh", ["γυνη"], app1 d3gynh),
     ("w-paideuw", ["παιδευω"], app1 vwpaideuw),
     ("prep", ["απο"], app1 preposition),
     ("particle", ["και"], app1 particle),
     ("adverb", ["καλως"], app1 adverb),
     ("mi-didwmi", ["διδωμι"], app1 vmididwmi)
    ]

{-
commands = 
 [
  ("d1rosa", ["rosa"], app1 d1rosa),
  ("d1poeta", ["poeta"], app1 d1poeta),
  ("d2servus",["servus"], app1 d2servus),
  ("d2pinus", ["pinus"], app1 d2pinus),
  ("d2virus",["virus"], app1 d2virus),
  ("d2bellum",["bellum"], app1 d2bellum),
  ("d2puer",["puer"], app1 d2puer),
  ("d2liber",["liber"], app1 d2liber),
  ("prep",   ["ad"], app1 prep),
  ("v1amare", ["amare"], app1 v1amare),
  ("v2habere", ["habere"], app1 v2habere)
  ]
-}
