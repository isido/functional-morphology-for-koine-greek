{-
    Functional Morphology: Greek paradigm definitions
    Ilja Sidoroff (ilja.sidoroff@iki.fi)

    Based on Latin paradigm definitions by
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


{- Definition of inflections -}
module RulesGreek where

import TypesGreek
import General

{- Interface functions. -}

type DictForm = String
type Stem     = String


{-- Nouns --}

-- First declension

decl1timh :: DictForm -> Noun
decl1timh timh (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> timh
                          Accusative -> tim ++ "ην"
                          Genitive -> tim ++ "ηs"
                          Dative -> tim ++ "ῃ"
                          Vocative -> timh
            Plural ->   case c of
                          Nominative -> tim ++ "αι"
                          Accusative -> tim ++ "ας"
                          Genitive -> tim ++ "ων"
                          Dative -> tim ++ "αις"
                          Vocative -> tim ++ "αι"
    where
      tim = tk 1 timh

decl1thalatta :: DictForm -> Noun
decl1thalatta thalatta (NounForm n c) =
    mkStr $
        case n of
           Singular -> case c of
                         Nominative -> thalatta
                         Accusative -> thalatt ++ "αν"
                         Genitive -> thalatt ++ "ας"
                         Dative -> thalatt ++ "ῃ"
                         Vocative -> thalatta
           Plural ->  case c of
                         Nominative -> thalatt ++ "αι"
                         Accusative -> thalatt ++ "ας"
                         Genitive -> thalatt ++ "ων"
                         Dative -> thalatt ++ "αις"
                         Vocative -> thalatta
     where
       thalatt = tk 2 thalatta
           

decl1hora :: DictForm -> Noun
decl1hora hora (NounForm n c) =
    mkStr $
        case n of
          Singular -> case c of
                        Nominative -> hora
                        Accusative -> hor ++ "αν"
                        Genitive -> hor ++ "ας"
                        Dative -> hor ++ "ᾳ"
                        Vocative -> hora
          Plural   -> case c of
                        Nominative -> hor ++ "αι"
                        Accusative -> hor ++ "ας"
                        Genitive -> hor ++ "ων"
                        Dative -> hor ++ "αις"
                        Vocative -> hora
    where
      hor = tk 1 hora


decl1krites :: DictForm -> Noun
decl1krites krites (NounForm n c) =
    mkStr $
        case n of
          Singular -> case c of
                        Nominative -> krites
                        Accusative -> krit ++ "ην"
                        Genitive -> krit ++ "ου"
                        Dative -> krit ++ "ῃ"
                        Vocative -> krit ++ "α" 
                      -- missing vocative when stem doesn't end with t (η)
          Plural   -> case c of
                        Nominative -> krit ++ "αι"
                        Accusative -> krit ++ "ας"
                        Genitive -> krit ++ "ων"
                        Dative -> krit ++ "αις"
                        Vocative -> krit ++ "αι"
    where
      krit = tk 2 krites

decl1neanias :: DictForm -> Noun
decl1neanias neanias (NounForm n c) =
    mkStr $
        case n of
          Singular -> case c of
                        Nominative -> neanias
                        Accusative -> neani ++ "αν"
                        Genitive -> neani ++ "ου"
                        Dative -> neani ++ "ᾳ"
                        Vocative -> neani ++ "α"
          Plural   -> case c of
                        Nominative -> neani ++ "αι"
                        Accusative -> neani++ "ας"
                        Genitive -> neani ++ "ων"
                        Dative -> neani ++ "αις"
                        Vocative -> neani ++ "αι"
    where
      neani = tk 2 neanias


-- Second declension

decl2logos :: DictForm -> Noun
decl2logos logos (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> logos
                          Accusative -> log ++ "ον"
                          Genitive -> log ++ "ου"
                          Dative -> log ++ "ῳ"
                          Vocative -> log ++ "ε"
            Plural   -> case c of
                          Nominative -> log ++ "οι"
                          Accusative -> log ++ "ους"
                          Genitive -> log ++ "ων"
                          Dative -> log ++ "οις"
                          Vocative -> log ++ "οι"
    where
      log = tk 2 logos

decl2dwron :: DictForm -> Noun
decl2dwron dwron (NounForm n c) =
    mkStr $
          case n of 
            Singular -> case c of 
                          Nominative -> dwron
                          Accusative -> dwron
                          Genitive -> dwr ++ "ου"
                          Dative -> dwr ++ "ῳ"
                          Vocative -> dwron
            Plural   -> case c of
                          Nominative -> dwr ++ "α"
                          Accusative -> dwr ++ "α"
                          Genitive -> dwr ++ "wn"
                          Dative -> dwr ++ "οις"
                          Vocative -> dwr ++ "α"
    where
      dwr = tk 2 dwron

decl2nous :: DictForm -> Noun
decl2nous nous (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> nous
                          Accusative -> _n ++ "ουν"
                          Genitive -> _n ++ "ου"
                          Dative -> _n ++ "ῳ"
                          Vocative -> nous
            Plural   -> case c of
                          Nominative -> _n ++ "οι"
                          Accusative -> nous
                          Genitive -> _n ++ "ων"
                          Dative -> _n ++ "οις"
                          Vocative -> _n ++ "οι"
    where
      _n = tk 3 nous

decl2ostoun :: DictForm -> Noun
decl2ostoun ostoun (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of 
                          Nominative -> ostoun
                          Accusative -> ostoun
                          Genitive -> ost ++ "ου"
                          Dative -> ost ++ "ῳ"
                          Vocative -> ostoun
            Plural ->   case c of
                          Nominative -> ost ++ "α"
                          Accusative -> ost ++ "α"
                          Genitive -> ost ++ "ων"
                          Dative -> ost ++ "οις"
                          Vocative -> ost ++ "α"
    where
      ost = tk 3 ostoun


-- Third Declension

decl3fylax :: DictForm -> Noun
decl3fylax fylax (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> fylax
                          Accusative -> fylak ++ "α"
                          Genitive -> fylak ++ "ος"
                          Dative -> fylak ++ "ι"
                          Vocative -> fylax
            Plural   -> case c of
                          Nominative -> fylak ++ "ες"
                          Accusative -> fylak ++ "ας"
                          Genitive -> fylak ++ "ων"
                          Dative -> fylax ++ "ι"
                          Vocative -> fylak ++ "ες"
          where
            fylak = tk 1 fylax ++ "κ"


decl3swma :: DictForm -> Noun
decl3swma swma (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> swma
                          Accusative -> swma
                          Genitive -> swma ++ "τος"
                          Dative -> swma ++ "τι"
                          Vocative -> swma
            Plural   -> case c of
                          Nominative -> swma ++ "τα"
                          Accusative -> swma ++ "τα"
                          Genitive -> swma ++ "των"
                          Dative -> swma ++ "σι"
                          Vocative -> swma ++ "τα"

decl3gerwn :: DictForm -> Noun
decl3gerwn gerwn (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> gerwn
                          Accusative -> ger ++ "οντα"
                          Genitive -> ger ++ "οντος"
                          Dative -> ger ++ "οντι"
                          Vocative -> ger ++ "ον"
            Plural   -> case c of
                          Nominative -> ger ++ "οντες"
                          Accusative -> ger ++ "οντας"
                          Genitive -> ger ++ "οντων"
                          Dative -> ger ++ "ουσι"
                          Vocative -> ger ++ "οντες"
          where
            ger = tk 2 gerwn
           
decl3anhr :: DictForm -> Noun
decl3anhr anhr (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> anhr
                          Accusative -> andr ++ "α"
                          Genitive -> andr ++ "ος"
                          Dative -> andr ++ "ι"
                          Vocative -> an ++ "ερ"
            Plural   -> case c of
                          Nominative -> andr ++ "ες"
                          Accusative -> andr ++ "ας"
                          Genitive -> andr ++ "ων"
                          Dative -> andr ++ "σι"
                          Vocative -> andr ++ "ες"
          where
            andr = tk 2 anhr ++ "δρ"
            an = tk 2 anhr

decl3pathr :: DictForm -> Noun
decl3pathr pathr (NounForm n c) =
  mkStr $
          case n of
            Singular -> case c of
                          Nominative -> pathr
                          Accusative -> pat ++ "ερα"
                          Genitive -> pat ++ "ρος"
                          Dative -> pat ++ "ρι"
                          Vocative -> pat ++ "ερ"
            Plural   -> case c of
                          Nominative -> pat ++ "ερες"
                          Accusative -> pat ++ "ερας"
                          Genitive -> pat ++ "ερων"
                          Dative -> pat ++ "ρασι"
                          Vocative -> pat ++ "ερες"
          where
            pat = tk 2 pathr


decl3elpis :: DictForm -> Noun
decl3elpis elpis (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> elpis
                          Accusative -> elpi ++ "δα"
                          Genitive -> elpi ++ "δος"
                          Dative -> elpi ++ "δι"
                          Vocative -> elpis
            Plural   -> case c of
                          Nominative -> elpi ++ "δες"
                          Accusative -> elpi ++ "δας"
                          Genitive -> elpi ++ "δων"
                          Dative -> elpi ++ "σι"
                          Vocative -> elpi ++ "δες"
          where
            elpi = tk 1 elpis


decl3rhtwr :: DictForm -> Noun
decl3rhtwr rhtwr (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of 
                          Nominative -> rhtwr
                          Accusative -> rht ++ "ορα"
                          Genitive -> rht ++ "ορος"
                          Dative -> rht ++ "ορι"
                          Vocative -> rht ++ "ορ"
            Plural   -> case c of
                          Nominative -> rht ++ "ορες"
                          Accusative -> rht ++ "ορας"
                          Genitive -> rht ++ "ορων"
                          Dative -> rht ++ "ορσι"
                          Vocative -> rht ++ "ορες"
          where
            rht = tk 2 rhtwr
                          

decl3gynh :: DictForm -> Noun
decl3gynh gynh (NounForm n c) =
    mkStr $
          case n of
            Singular -> case c of
                          Nominative -> gynh
                          Accusative -> gyn ++ "αικα"
                          Genitive -> gyn ++ "αικος"
                          Dative -> gyn ++ "αικι"
                          Vocative -> gyn ++ "αι"
            Plural   -> case c of
                          Nominative -> gyn ++ "αικες"
                          Accusative -> gyn ++ "αικας"
                          Genitive -> gyn ++ "αικων"
                          Dative -> gyn ++ "αιξι"
                          Vocative -> gyn ++ "αικες"
          where
            gyn = tk 1 gynh

{- Adjectives -}

decl12Adj :: String ->  (String -> (Gender, Number, Case) -> Str) -> Adjective
decl12Adj sofos decl (AdjectiveForm gr g n c) =
    case gr of
      Positive ->
          decl sofos (g, n, c)
      Comparative ->
          decl sofoteros (g, n, c)
      Superlative ->
          decl sofotatos (g, n, c)
    where
      sofoteros = tk 1 sofos ++ "τερος"
      sofotatos = tk 1 sofos ++ "τατος"



{-
decl1Adj :: String -> String -> String -> (String -> (Gender,Number,Case) -> Str) -> Adjective
decl1Adj bonus melior optimus decl (AdjectiveForm gr g n c) =
    case gr of
     Positive ->
	 decl bonus (g,n,c)
     Comparative -> 
	 declfortior melior (g,n,c)
     Superlative -> decl optimus (g,n,c)

decl1bonus :: String -> (Gender,Number,Case) -> Str
decl1bonus bonus = decl1aux bonus (tk 2 bonus) 

decl1tener :: String -> (Gender,Number,Case) -> Str
decl1tener tener = decl1aux tener tener

decl1sacer :: String -> (Gender,Number,Case) -> Str
decl1sacer sacer = decl1aux sacer (tk 2 sacer ++ "r")

decl1aux :: String -> String -> (Gender,Number,Case) -> Str
decl1aux bonus bon (g,n,c) =
    case g of
	  Masculine ->
	   decl2logos bonus (NounForm n c)
	  Feminine  ->
	   decl1timh (bon ++ "a") (NounForm n c)
	  Neuter   -> 
	   decl2dwron (bon ++ "um") (NounForm n c)

declfortior :: String -> (Gender,Number,Case) -> Str
declfortior fortior (g,n,c) = 
 mkStr $
   case g of
    Neuter -> case n of
	       Singular -> 
		   case c of
		    Genitive -> fortioris
		    Dative   -> fortiori
		    _        -> forti   ++ "us"
	       Plural ->
	           case c of
	            Genitive -> fortiorum
		    Dative   -> fortioribus
		    _        -> fortior ++ "a"
    _     ->  case n of
	       Singular -> 
		   case c of
		    Accusative -> fortior ++ "em"
		    Genitive -> fortioris
		    Dative   -> fortiori
		    _        -> fortior
	       Plural ->
	           case c of
	            Genitive -> fortiorum
		    Dative   -> fortioribus
		    _        -> fortior ++ "es"
  where forti       = tk 2 fortior
        fortioribus = fortior ++ "ibus"
	fortiorum   = fortior ++ "um"
	fortioris   = fortior ++ "is"
        fortiori    = fortior ++ "i"
	fortiore    = fortior ++ "e"

-}
-- Adverb
-- 

mkAdverb :: String -> Adverb
mkAdverb kalws (AdverbForm gr) =
    case gr of
        Positive    -> mkStr $ kalws
        Comparative -> mkStr $ kalw ++ "τερον"
        Superlative -> mkStr $ kalw ++ "τατα"
    where
      kalw = tk 1 kalws

mkAdverbIrreg :: String -> String -> String -> Adverb
mkAdverbIrreg mala mallon malista = mkStr1 $ giveValues [mala, mallon, malista]

-- Prepositions

mkPreposition :: String -> Preposition
mkPreposition s _ = mkStr s

-- Particles

mkParticle :: String -> Particle
mkParticle s _ = mkStr s

------------------------- Verbs

type PresentStem    = String
type PerfectStem    = String
type SubjStem       = String
type Infinitive     = String
type Participle     = String

vPaideuw :: String -> Verb
vPaideuw paideuw vf =
   case vf of
     Indicative p n Present v -> mkStr $ endingsPresentW paideu (p, n, v)
     Subjunctive p n PresentI v -> mkStr $ endingsPresentSubjunctive paideu (p, n, v)
     Imperative p n PresentI v -> mkStr $ endingsPresentImperativeW paideu (p, n, v)
     Optative p n PresentI v -> mkStr $ endingsOptative paideu (p, n, v)
     Indicative p n Imperfect v -> mkStr $ augment (endingsImperfectW paideu (p, n, v))
     Infinitive t v           -> mkInfinitive paideu (t, v)
     Indicative p n Aorist v -> mkStr $ augment (endingsSigmaticAorist paideus (p, n, v))
     Subjunctive p n AoristI v -> mkStr $ endingsPresentSubjunctive paideus (p, n, v)
     Indicative p n Future v -> mkStr $ endingsPresentW paideus (p, n, v)
     Participle t v n c g -> mkParticiple paideuw t v n c g
     _ -> mkStr $ paideuw
   
   where
 --    stem = mkStem t v paideuw

     paideu = tk 1 paideuw
     paideus = tk 1 paideuw ++ "σ" -- todo: fix this for general case


vTimaw :: String -> Verb
vTimaw timaw vf =
    case vf of
      Indicative p n Present v -> mkStr $ endingsPresentAW tim (p, n, v)
      _ -> mkStr $ tim
    where
      tim = tk 2 timaw

vFilew :: String -> Verb
vFilew filew vf =
    case vf of
      Indicative p n Present v -> mkStr $ endingsPresentAW fil (p, n, v)
      _ -> mkStr $ fil
    where
      fil = tk 2 filew

vDhlow :: String -> Verb
vDhlow dhlow vf =
    case vf of
      Indicative p n Present v -> mkStr $ endingsPresentAW dhl (p, n, v)
      _ -> mkStr $ dhl
    where
      dhl = tk 2 dhlow

vDidwmi :: String -> Verb
vDidwmi didwmi vf =
   mkStr $ didwmi

-- Note : there exist a change e -> i

vHabere :: String -> Verb
vHabere habere vf =
  case vf of
   Indicative First Singular Future Active  -> mkStr $ habe ++ "bo"
   _ -> mkStr $ habere
   {-
   Indicative p n t Active                  -> mkIndicativeActive habe (hab ++ "u") II (p,n,t)
   Indicative First Singular Present Passive -> mkStr $ habe ++ "or"
   Indicative First Singular Future Passive -> mkStr $ habe ++ "bor"
   Indicative Third Plural   Future Passive -> mkStr $ habe ++ "buntur"
   Indicative p n t Passive                 -> mkIndicativePassive habe (hab ++ "it") II (p,n,t) 
   Subjunctive p n t Active                 -> mkSubjunctiveActive (hab ++ "ea") (hab ++ "it") habere (p,n,t)
   Subjunctive p n t Passive                -> mkSubjunctivePassive (hab ++ "ea") (hab ++ "u") habere (p,n,t) 
   Infinitive t v                           -> mkInfinitive habe (t,v)
   ImperativePresent n v                    -> mkImperativePresent habe (n,v)
   ImperativeFutureActive  n p   -> mkImperativeFutureActive habe (n,p)
   ImperativeFuturePassiveSing p -> 
    mkStr $   
     case p of
      _ -> habe ++ "tor" 
   ImperativeFuturePassivePl -> mkStr $ habe ++ "ntor"
   ParticiplesFuture  v ->
    mkStr $
      case v of
       Active -> hab ++ "iturus"
       Passive -> habe ++ "ndus"
   ParticiplesPresent -> mkStr $ habe ++ "ns"
   ParticiplesPerfect -> mkStr $ hab ++ "itus"
   GerundGenitive -> mkStr $ habe ++ "ndi"
   GerundDative   -> mkStr $ habe ++ "ndo"
   GerundAcc      -> mkStr $ habe ++ "ndum"
   GerundAbl      -> mkStr $ habe ++ "ndo"
   SupineAcc      -> mkStr $ hab  ++ "ito"
   SupineAblative -> mkStr $ hab  ++ "itu" -}
 where habe = tk 2 habere
       hab  = tk 1 habe

vAmare :: String -> Verb
vAmare amare vf =
  case vf of
   Indicative First Singular Present Active  -> mkStr $ am ++ "o"
   _ -> mkStr $ amare
   {-
   Indicative p n t Active  -> mkIndicativeActive ama (ama ++ "v") I (p,n,t) 
   Indicative First Singular Present Passive  -> mkStr $ am ++ "or"
   Indicative First Singular Future Passive -> mkStr $ ama ++ "bor"
   Indicative p n t Passive  -> mkIndicativePassive ama (ama ++ "t") I (p,n,t) 
   Subjunctive p n t Active  -> mkSubjunctiveActive (am ++ "e") (ama ++ "v") amare (p,n,t)
   Subjunctive p n t Passive -> mkSubjunctivePassive (am ++ "e") (ama ++ "t") amare (p,n,t)
   Infinitive t v            -> mkInfinitive ama (t,v)
   ImperativePresent n v     -> mkImperativePresent ama (n,v)
   ImperativeFutureActive  n p   -> mkImperativeFutureActive ama (n,p)
   ImperativeFuturePassiveSing p ->  
    mkStr $  
     case p of
      _ -> ama ++ "tor" 
   ImperativeFuturePassivePl -> mkStr $ ama ++ "ntor"
   ParticiplesFuture  v ->
    mkStr $
      case v of
       Active -> ama ++ "turus"
       Passive -> ama ++ "ndus"
   ParticiplesPresent -> mkStr $ ama ++ "ns"
   ParticiplesPerfect -> mkStr $ ama ++ "tus"
   GerundGenitive -> mkStr $ ama ++ "ndi"
   GerundDative   -> mkStr $ ama ++ "ndo"
   GerundAcc      -> mkStr $ ama ++ "ndum"
   GerundAbl      -> mkStr $ ama ++ "ndo"
   SupineAcc      -> mkStr $ ama ++ "tum"
   SupineAblative -> mkStr $ ama ++ "tu" -}
 where ama = tk 2 amare
       am  = tk 1 ama


-- make stems
mkStem :: String -> Tense -> Voice -> String
mkStem verb t v =
    case (t,v) of
    (Present, _)      -> tk 1 verb
    (Imperfect, _)    -> tk 1 verb
    (Aorist, Passive) -> mkSigmaticAoristStem verb
    (Aorist, _)       -> mkSigmaticAoristStem verb
    (Future, Passive) -> mkSigmaticAoristStem verb
    (Future, _)       -> mkSigmaticAoristStem verb
    (Perfect, _)      -> reduplicate (tk 1 verb)
    (PluPerfect, _)   -> augment (reduplicate (tk 1 verb))


mkWStem :: String -> TenseI -> String
mkWStem verb t =
    case t of
      PresentI -> tk 1 verb
      AoristI  -> mkSigmaticAoristStem verb
      PerfectI -> tk 1 verb ++ "κ"
      FutureI  -> tk 1 verb ++ "σ"

mkSigmaticAoristStem :: String -> String
mkSigmaticAoristStem verb =
    case lastc of
      'π' -> tk 2 verb ++ "ψ"
      'β' -> tk 2 verb ++ "ψ"
      'φ' -> tk 2 verb ++ "ψ"                       
      _ -> tk 1 verb ++ "σ"
      
    where
      lastc = last verb

-- participle
mkParticiple :: String -> TenseI -> Voice -> Number -> Case -> Gender -> Str
mkParticiple part t v n c g =
    case v of
      Active -> 
          case g of
            Masculine -> decl3gerwn (stem ++ "ων") (NounForm n c)
            Neuter    -> decl3gerwn (stem ++ "ον") (NounForm n c)
            Feminine  -> decl1thalatta (stem ++ "ουσα") (NounForm n c)
      _   ->
          case g of
            Masculine -> decl2logos (stem ++ "ομενος") (NounForm n c)
            Neuter    -> decl2dwron (stem ++ "ομενον") (NounForm n c)
            Feminine  -> decl1timh  (stem ++ "ομενη") (NounForm n c)

    where
      stem = mkWStem part t

-- personal endings for ω-verbs in present tense
endingsPresentW :: String -> (Person, Number, Voice) -> String
endingsPresentW prStem (p, n, v) =
    case v of
       Active ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ω"
           (Singular,Second) -> prStem ++ "εις"
           (Singular,Third)  -> prStem ++ "ει"
           (Plural,First)    -> prStem ++ "ομεν"
           (Plural,Second)   -> prStem ++ "ετε"
           (Plural,Third)    -> prStem ++ "ουσι"
       _ ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ομαι"
           (Singular,Second) -> prStem ++ "ῃ"
           (Singular,Third)  -> prStem ++ "εται"
           (Plural,First)    -> prStem ++ "ομεθα"
           (Plural,Second)   -> prStem ++ "εσθε"
           (Plural,Third)    -> prStem ++ "ονται"

endingsPresentImperativeW :: String -> (PersonI, Number, Voice) -> String
endingsPresentImperativeW prStem (p, n, v) =
    case v of
      Active ->
          case (n, p) of
            (Singular, SecondI) -> prStem ++ "ε"
            (Singular, ThirdI)  -> prStem ++ "ετω"
            (Plural, SecondI)   -> prStem ++ "ετε"
            (Plural, ThirdI)    -> prStem ++ "οντων"
      _ ->
          case (n, p) of
            (Singular, SecondI) -> prStem ++ "ου"
            (Singular, ThirdI)  -> prStem ++ "εσθω"
            (Plural, SecondI)   -> prStem ++ "εσθε"
            (Plural, ThirdI)    -> prStem ++ "εσθων"

endingsPresentSubjunctive :: String -> (Person, Number, Voice) -> String
endingsPresentSubjunctive stem (p, n, v) =
    case v of
         Active ->
             case (n,p) of
               (Singular,First)  -> stem ++ "ω"
               (Singular,Second) -> stem ++ "ῃς"
               (Singular,Third)  -> stem ++ "ῃ"
               (Plural,First)    -> stem ++ "ωμεν"
               (Plural,Second)   -> stem ++ "ητε"
               (Plural,Third)    -> stem ++ "ωσι"
         _ ->
             case (n,p) of
               (Singular,First)  -> stem ++ "ωμαι"
               (Singular,Second) -> stem ++ "ῃ"
               (Singular,Third)  -> stem ++ "ηται"
               (Plural,First)    -> stem ++ "ωμεθα"
               (Plural,Second)   -> stem ++ "ησθε"
               (Plural,Third)    -> stem ++ "ωνται"

-- personal endings for ω-verbs in imperfect tense
endingsImperfectW :: String -> (Person, Number, Voice) -> String
endingsImperfectW prStem (p, n, v) =
    case v of 
      Active ->
          case (n,p) of
           (Singular,First)  -> prStem ++ "ον"
           (Singular,Second) -> prStem ++ "ες"
           (Singular,Third)  -> prStem ++ "ε"
           (Plural,First)    -> prStem ++ "ομεν"
           (Plural,Second)   -> prStem ++ "ετε"
           (Plural,Third)    -> prStem ++ "ον"
      _ ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ομην"
           (Singular,Second) -> prStem ++ "ου"
           (Singular,Third)  -> prStem ++ "ετο"
           (Plural,First)    -> prStem ++ "ομεθα"
           (Plural,Second)   -> prStem ++ "εσθε"
           (Plural,Third)    -> prStem ++ "οντο"

-- personal endings for optative (present, future, perfect)
endingsOptative :: String -> (Person, Number, Voice) -> String
endingsOptative stem (p, n, v) =
    case v of
      Active ->
          case (n, p) of
            (Singular, First)  -> stem ++ "οιμι"
            (Singular, Second) -> stem ++ "οις"
            (Singular, Third)  -> stem ++ "οι"
            (Plural, First)    -> stem ++ "οιμεν"
            (Plural, Second)   -> stem ++ "οιτε"
            (Plural, Third)    -> stem ++ "οιεν"
      _ ->
          case (n, p) of
            (Singular, First)  -> stem ++ "οιμην"
            (Singular, Second) -> stem ++ "οιο"
            (Singular, Third)  -> stem ++ "οιτο"
            (Plural, First)    -> stem ++ "οιμεθα"
            (Plural, Second)   -> stem ++ "οισθε"
            (Plural, Third)    -> stem ++ "οιντο"


-- personal endings for sigmatic aorist
endingsSigmaticAorist :: String -> (Person, Number, Voice) -> String
endingsSigmaticAorist stem (p, n, v) =
    case v of 
      Active ->
          case (n,p) of
           (Singular,First)  -> stem ++ "α"
           (Singular,Second) -> stem ++ "ας"
           (Singular,Third)  -> stem ++ "ε"
           (Plural,First)    -> stem ++ "αμεν"
           (Plural,Second)   -> stem ++ "ατε"
           (Plural,Third)    -> stem ++ "αν"
      _ ->
         case (n,p) of
           (Singular,First)  -> stem ++ "αμην"
           (Singular,Second) -> stem ++ "ω"
           (Singular,Third)  -> stem ++ "ατο"
           (Plural,First)    -> stem ++ "αμεθα"
           (Plural,Second)   -> stem ++ "ασθε"
           (Plural,Third)    -> stem ++ "αντο"

-- presonal endings for contracted ω-verbs in present tense
endingsPresentAW :: String -> (Person, Number, Voice) -> String
endingsPresentAW prStem (p, n, v) =
    case v of
       Active ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ῶ"
           (Singular,Second) -> prStem ++ "ᾷς"
           (Singular,Third)  -> prStem ++ "ᾷ"
           (Plural,First)    -> prStem ++ "ῶμεν"
           (Plural,Second)   -> prStem ++ "ᾶτε"
           (Plural,Third)    -> prStem ++ "ῶσι"
       _ ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ῶμαι"
           (Singular,Second) -> prStem ++ "ᾷ"
           (Singular,Third)  -> prStem ++ "ᾶται"
           (Plural,First)    -> prStem ++ "ῶμεθα"
           (Plural,Second)   -> prStem ++ "ᾶσθε"
           (Plural,Third)    -> prStem ++ "ῶνται"

endingsPresentEW :: String -> (Person, Number, Voice) -> String
endingsPresentEW prStem (p, n, v) =
    case v of
       Active ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ῶ"
           (Singular,Second) -> prStem ++ "εῖς"
           (Singular,Third)  -> prStem ++ "εῖ"
           (Plural,First)    -> prStem ++ "οῦμεν"
           (Plural,Second)   -> prStem ++ "εῖτε"
           (Plural,Third)    -> prStem ++ "οῦσι"
       _ ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "οῦμαι"
           (Singular,Second) -> prStem ++ "εῖ" -- or "ῇ"
           (Singular,Third)  -> prStem ++ "εῖται"
           (Plural,First)    -> prStem ++ "οῦμεθα"
           (Plural,Second)   -> prStem ++ "εῖσθε"
           (Plural,Third)    -> prStem ++ "οῦνται"

endingsPresentOW :: String -> (Person, Number, Voice) -> String
endingsPresentOW prStem (p, n, v) =
    case v of
       Active ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "ῶ"
           (Singular,Second) -> prStem ++ "οῖς"
           (Singular,Third)  -> prStem ++ "οῖ"
           (Plural,First)    -> prStem ++ "οῦμεν"
           (Plural,Second)   -> prStem ++ "οῦτε"
           (Plural,Third)    -> prStem ++ "οῦσι"
       _ ->
         case (n,p) of
           (Singular,First)  -> prStem ++ "οῦμαι"
           (Singular,Second) -> prStem ++ "οῖ"
           (Singular,Third)  -> prStem ++ "οῦται"
           (Plural,First)    -> prStem ++ "οῦμεθα"
           (Plural,Second)   -> prStem ++ "οῦσθε"
           (Plural,Third)    -> prStem ++ "οῦνται"

-- personal endings for μι-verbs in present tense          
endingsPresentMi :: String -> (Person, Number, Voice) -> String
endingsPresentMi prStem (p, n, v) =
    case v of
      Active ->
        case (n,p) of
           (Singular,First)  -> prStem ++ "μι"
           (Singular,Second) -> prStem ++ "ς"
           (Singular,Third)  -> prStem ++ "σι"
           (Plural,First)    -> prStem ++ "μεν"
           (Plural,Second)   -> prStem ++ "τε"
           (Plural,Third)    -> prStem ++ "ασι"
      _ ->
        case (n,p) of
           (Singular,First)  -> prStem ++ "μαι"
           (Singular,Second) -> prStem ++ "σαι"
           (Singular,Third)  -> prStem ++ "ται"
           (Plural,First)    -> prStem ++ "μεθα"
           (Plural,Second)   -> prStem ++ "σθε"
           (Plural,Third)    -> prStem ++ "νται"
           
-- personal endings for all conjugations in present tense
endingsActive :: String -> Number -> Person -> String
endingsActive prStem n p = 
    case (n,p) of
     (Singular,First)  -> prStem ++ "o"
     (Singular,Second) -> prStem ++ "s"
     (Singular,Third)  -> prStem ++ "t"
     (Plural,First)    -> prStem ++ "mus"
     (Plural,Second)   -> prStem ++ "tis"
     (Plural,Third)    -> prStem ++ "nt"

-- personal endings for all conjugations in imperfect
endingsImperfect :: String -> Number -> Person -> String
endingsImperfect prStem n p = 
    case (n,p) of
     (Singular,First)  -> prStem ++ "m"
     _                 -> endingsActive prStem n p


{-
mkSubjunctiveActive :: SubjStem -> PerfectStem -> Infinitive -> (Person, Number, TenseS) -> Str
mkSubjunctiveActive subj peStem infin (p,n,t) =
    case t of
     PresentS       -> mkStr $ endingsImperfect subj n p
     ImperfectS     -> mkStr $ endingsImperfect infin n p
     PerfectS       -> mkStr $ endingsImperfect (peStem ++ "eri") n p
     PluPerfectS    -> mkStr $ endingsImperfect (peStem ++ "isse") n p

mkSubjunctivePassive :: SubjStem -> Participle -> Infinitive -> (Person, Number, TenseS) -> Str
mkSubjunctivePassive subj part infin (p,n,t) =
    case t of
     PresentS       -> mkStr $ subj ++ endingsPassive n p
     ImperfectS     -> mkStr $ infin ++ endingsPassive n p
     _              -> mkStr $ case n of 
			        Singular -> part ++ "us"
			        Plural   -> part ++ "i"

mkIndicativeActive :: PresentStem -> PerfectStem -> Conjugation -> (Person, Number, Tense) -> Str
mkIndicativeActive prStem peStem c (p,n,t) = 
    case t of
     Present       -> mkStr $ endingsActive prStem n p 
     Imperfect     -> mkStr $ endingsImperfect (prStem ++ "ba") n p
     Perfect       -> mkStr $ peStem ++ endingsPerfect n p
     PluPerfect    -> mkStr $ endingsImperfect (peStem ++ "era") n p
     Future        -> mkStr $ 
		       case c of
		        I  -> case (n,p) of
			       (Plural,Third) -> prStem ++ "bunt"
			       _              -> endingsActive (prStem ++ "bi") n p
			II -> endingsActive (prStem ++ "bi") n p
			_  -> endingsImperfect (prStem ++ "e") n p

mkIndicativePassive :: PresentStem -> Participle -> Conjugation -> (Person, Number, Tense) -> Str
mkIndicativePassive prStem part c (p,n,t) = 
    case t of
     Present       -> mkStr $ prStem ++ endingsPassive n p 
     Imperfect     -> mkStr $ prStem ++ "ba" ++ endingsPassive n p
     Future        -> mkStr $ 
		       case c of
		        I  -> prStem ++ "bi" ++ endingsPassive n p
			II -> prStem ++ "bi" ++ endingsPassive n p
			_  -> prStem ++ "e" ++ endingsPassive n p
     _             -> mkStr $ case n of 
			       Singular -> part ++ "us"
			       Plural   -> part ++ "i"
-}

mkInfinitive :: PresentStem -> (TenseI, Voice) -> Str
mkInfinitive prStem (t,v) = mkStr $
    case (t,v) of
     (PresentI, Active)  -> prStem ++ "ειν"
     (PresentI, _)       -> prStem ++ "εσθαι"
     (_,_)               -> prStem
     {-
     (PerfectI, Active)  -> prStem ++ "visse"
     (FutureI, Active)   -> prStem ++ "turus"
     (PresentI, Passive) -> prStem ++ "ri"
     (PerfectI, Passive) -> prStem ++ "tus"
     (FutureI, Passive)  -> prStem ++ "tum" -}

{-
mkImperativePresent :: PresentStem -> (Number,Voice) -> Str
mkImperativePresent prStem (n,v) =
  mkStr $ 
    case (n,v) of
     (Singular, Active)  -> prStem 
     (Plural, Active)    -> prStem ++ "te"
     (Singular, Passive) -> prStem ++ "re"
     (Plural, Passive)   -> prStem ++ "mini"

mkImperativeFutureActive :: PresentStem -> (Number,PersonI) -> Str
mkImperativeFutureActive prStem (n,p) =
 mkStr $
  case (n,p) of
   (Plural,SecondI)   -> prStem ++ "tote"
   (Plural,ThirdI)    -> prStem ++ "nto"
   _                  -> prStem ++ "to"
--}


--- Augmentation
augment :: String -> String
augment word =
  if isVowel c then
      case cc of
        "αι"  -> "ῃ" ++ drop 2 word
        "ει"  -> "ῃ" ++ drop 2 word
        "αυ" -> "ηυ" ++ drop 2 word
        "ευ" -> "ηυ" ++ drop 2 word
        "οι" -> "ῳ" ++ drop 2 word
        _ ->
            case c of
              'α' -> "η" ++ tail word
              'ᾳ' -> "ῃ" ++ tail word
              'ε' -> "η" ++ tail word
              'ο' -> "ω" ++ tail word
              _ -> word
  else
      case c of
          'ρ' -> "ερ" ++ word    
          _ -> "ε" ++ word
    where
      c = head word
      cc = dp 2 word    

reduplicate :: String -> String
reduplicate word =
    if isVowel c then
        augment word
    else
        show c ++  "ε" ++ word
    where
      c = head word



isVowel :: Char -> Bool
isVowel c = elem c "αεηιοωυ"
