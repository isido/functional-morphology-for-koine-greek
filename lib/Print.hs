module Print where

import General
import Dictionary
import List (intersperse,sort)
import IO
import Char
import Util
import qualified Data.Set as Set

--import UTF8

type Ident = String

-- printing morphological objects as strings

-- | Print word forms separated with '/'
prStr :: Str -> String
prStr = concat . intersperse "/" . unStr

-- | similar prStr, but output '*' for nonExist 
prAlts :: Str -> String
prAlts ss = 
    case unStr ss of
     [] -> "*"
     ys -> unwords $ intersperse "/" ys

-- | Create a constant table
consTable :: Str -> Table String
consTable s = [("INVAR", s)]

-- | Create an attributed constant table
consTableW :: Str -> [(String,(Attr,Str))]
consTableW s = [("INVAR", (noComp,s))]

-- | Output a 'showed' inflection function
putFun0 :: Param a => (a -> Str) -> IO ()
putFun0 = putStr . unlines . map show . prTable . table

-- | Output an inflection function
putFun :: Param a => (a -> Str) -> IO ()
putFun = putStr . unlines . map pr . prTable . table where
  pr (a,ss) = a ++ " : " ++ prAlts ss

-- | Print a parameter value without hierarchy (= parentheses)
prFlat :: String -> String
prFlat = filter (flip notElem "()")

-- | Show all values for the first parameter
prFirstForm :: Param a => Table a -> String
prFirstForm = prStr . firstForm

-- | Show one value for the first parameter (used in dictionary)
prDictForm :: Param a => Table a -> String
prDictForm = prDictStr . firstForm

-- | Another Str printing function
prDictStr :: Str -> String
prDictStr t = case unStr t of
  s:_ -> s
  []  -> "NONE"

-- | Print dictionary without attributes.
prDictionary :: Dictionary -> String
prDictionary = concat . intersperse ("\n") . map (unlines . prOne) . unDict
 where
  prOne (id,stem,para,typ,inhs,infl,_) = 
     let m     = maximum (map (length.fst) infl) + 1
         pad s = s ++ (take (m - length s) (repeat ' '))
       in
           "{" : (" paradigm : " ++ para) : (" head     : " ++ stem) : (" pos      : " ++ typ) : (" inhs     : " ++ (star (unwords inhs))) : "  {" :
                                       ["  " ++ pad (a) ++ ": " ++ star (prStr s) | (a,(_,s)) <- infl] ++ ["  }","}"]
  star [] = "*"
  star xs = xs

prParadigmsCompact :: Dictionary -> String
prParadigmsCompact = unlines . map prOne . unDict
 where
  prOne (id,stem,para,typ,inhs,infl,_) = 
    unlines [concat [para, " : ", typ, " : ",stem],
             concat (intersperse " | " [prStr s| (_,(_,s)) <- infl, not (null (unStr s))])]

-- | Print Dictionary in a structured format.
prNewDictionary :: Dictionary -> String
prNewDictionary = unlines . map prOne . unDict where
  prOne (id,stem, para, typ, inhs, infl,ex) = 
   concat ["{",
           prId id,
           prLemma stem "?" typ,
           prAdditional inhs,
           prTable infl,
           "};"]
  prId s = "id=\"" ++ s ++ "\""
  prLemma w para pos = concat ["lemma={word=\"", w, "\"paradigm=\"", para,
                               "\"pos=\"", pos, "\"}"]
  prAdditional as = "additional={" ++ 
                    concat [concat ["inh",n,"=\"",a, "\""] | (a,n) <- zip as (map show [1..])] ++ "}" 
  prTable xs =  "table={" ++ concat (map prE xs) ++ "}"
  prE (param, (attr,str)) = 
      concat ["entry={param=\"", param, "\"wfs={", prWord attr str,"}}"]
  prWord attr str = concat 
                    [concat ["wf={word=\"",w, 
                             "\"id=", show ("w"++n), 
                             "comp=", sAttr attr,"}"] | (w,n) <- zip (unStr str) (map show [1..])]
  sAttr = show . show                                    

prWordlist :: FullFormLex -> String
prWordlist = unlines . map fst

-- | Write a fullform lexicon 
prFullFormLex :: FullFormLex -> String
prFullFormLex []     = ""
prFullFormLex (x:xs) = prOne x ++ prFullFormLex xs
 where
  prOne (s,ps) = unlines [s ++ a | a <- map prAttr ps]
  prAttr (a,ss) = (':':ss) ++ prCompAttr a

prTabbedLex ::  Dictionary -> String
prTabbedLex = unlines . concat . map prOne . unDict
 where prOne (id,stem,_,wc,inhs,tbl,_) = 
                -- (stem,wc,inhs,tbl) = 
           [concat (intersperse "\t" [x, stem, wc, a ++ " " ++ unwords inhs, id]) | (a,(_,s)) <- tbl, not (null (unStr s)), x <- unStr s]

prWebService :: Dictionary -> String 
prWebService d = unlines $ reverse $ sort $ concat $ map prOne $ unDict d
 where pr [] = []
       pr xs = " " ++ unwords xs
       prOne (id,stem,_,wc,inhs,tbl,_) = 
           [concat (intersperse "\t" [x, stem, wc ++ " " ++  a ++ pr inhs]) | (a,(_,s)) <- tbl, not (null (unStr s)), x <- unStr s]



-- | Print attribute 
prCompAttr :: Attr -> String
prCompAttr a = " [" ++ show a ++ "] "

{-
-- |  Generate GF paradigm functions.
prGFRes :: Dictionary -> String
prGFRes = unlines . map prGFOper . unDict

prGFOper :: Entry -> String
prGFOper (oper',_, _, ty, inhs, tab0',_) 
 = begin ++ " : Str -> " ++ ty ++ " = " ++ bind ++ rec ++ end where
  oper  = case map undot oper' of
            ('_':xs) -> xs
            xs       -> xs
  tab0 = [(a,b) | (a,(_,b)) <- tab0']
  undot '.' = '_'
  undot x = x
  begin = "oper " ++ oper
  bind  = "\\" ++ oper ++ " -> " ++ 
            "\n  let " ++ 
            stemv ++ " = Predef.tk " ++ show lg1 ++ " " ++ oper ++ " in"
  stem  = longestPrefix (unStr (formsInTable tab0))
  stemv = if lg == 0 then "x_" else stem ++ "_" -- to avoid clash with res words
  lg1   = length oper - lg
  lg    = length stem
  tab   = mapInTable 
             (\w -> stemv ++ " + \"" ++ drop lg w ++ "\"") tab0
  rec   = "\n {s = " ++ tbl ++ 
          (if null inhs then "" else " ;\n  ") ++
          concat (intersperse " ;\n  " 
                    ["h" ++ show i ++ " = " ++ p | (i,p) <- zip [1..] inhs]
                 ) ++
          "\n  }"
  tbl = case tab of
    [("INVAR",ss)] -> altsGF ss --- a hack to avoid one-branch tables; name-sensit.
    _ -> "table {\n" ++ 
          concat (intersperse " ;\n" 
                    ["    "++ a ++ " => "++ altsGFRes b | (a,b) <- tab]
                 ) ++
          "\n    }"
  end   = " ;\n"
-}

gf_param :: String -> String -- should be extended to include all reserved words
gf_param s = unwords $ map f $ words s
 where f x = if isResGF x then x ++"'" else x

-- | Print GF source code.
prGF :: Dictionary -> String
prGF dict = cats ++ (unlines (map prGFRule (unDict dict)))
 where cs = unlines ["cat " ++ gf_param c ++ ";" | c <- map fst $ classifyDict dict]
       cats = "\n" ++ cs ++ "\n\n"

prGFRule ::  Entry -> String
prGFRule (id',_, _, cat, inhs, tab',_) =
  "fun " ++ name ++ " : " ++ (gf_param cat) ++ " ;\n\n" ++
  "lin " ++ name ++ " = {s = table {\n" ++ 
    concat (intersperse " ;\n" 
               ["  "++ a ++ " => "++ altsGF b | (a,b) <- tab]) ++
          (if null inhs then "}" else " };\n  ") ++
          concat (intersperse " ;\n  " 
                    ["h" ++ show i ++ " = " ++ p | (i,p) <- zip [1..] inhs]
                 ) ++
     "\n} ;\n"
 where tab = [(gf_param (map dash2us a),b) | (a,(_,b)) <- tab']
       dash2us '-' = '_'
       dash2us x = x
       num x = if isDigit (head x) then 'x':x else x
       name =  num $ dropWhile (== '_') $ transform_letters $ map dash2us $ dropWhile (== '_') $ undot id'
       transform_letters = concat . map trans
       trans 'å' = "aa"
       trans 'Å' = "AA"
       trans 'ä' = "ae"
       trans 'Ä' = "AE"
       trans 'à' = "a"
       trans 'á' = "a"
       trans 'é' = "e"
       trans 'è' = "e"
       trans 'ê' = "e"
       trans 'ç' = "c"
       trans 'ü' = "u"
       trans 'ø' = "oe"
       trans 'ñ' = "n"       
       trans 'Ø' = "OE"
       trans 'æ' = "ae"
       trans 'Æ' = "AE"
       trans 'ö' = "oe"
       trans 'Ö' = "OE"
       trans x   = [x]
       undot [] = []
       undot ('.':'.':xs) = '_' : undot xs 
       undot     ('.':xs) = '_' : undot xs
       undot       (x:xs) = x:undot xs


data BTree = N | B String BTree BTree deriving (Show)

isResGF :: String -> Bool
isResGF s = treeFind resWords
  where
  treeFind N = False
  treeFind (B a left right) | s < a  = treeFind left
                            | s > a  = treeFind right
                            | s == a = True

resWords = b "lincat" (b "def" (b "Type" (b "Str" (b "PType" (b "Lin" N N) N) (b "Tok" (b "Strs" N N) N)) (b "cat" (b "case" (b "abstract" N N) N) (b "data" (b "concrete" N N) N))) (b "include" (b "fun" (b "fn" (b "flags" N N) N) (b "in" (b "grammar" N N) N)) (b "interface" (b "instance" (b "incomplete" N N) N) (b "lin" (b "let" N N) N)))) (b "resource" (b "out" (b "of" (b "lintype" (b "lindef" N N) N) (b "oper" (b "open" N N) N)) (b "pattern" (b "param" (b "package" N N) N) (b "printname" (b "pre" N N) N))) (b "union" (b "table" (b "strs" (b "reuse" N N) N) (b "transfer" (b "tokenizer" N N) N)) (b "where" (b "variants" (b "var" N N) N) (b "with" N N))))
   where b s = B s 

-- | two GF modes for free variation; old for GF<0.98 
altsGF xs = case (unStr xs) of
  [x] -> prQ x
  ys -> "variants"++" {" ++ unwords (intersperse ";" (map prQ ys)) ++ "}" where
 where
   prQ s 
    | any isSpace s   = "[" ++ quote s ++ "]"
    | otherwise       = quote s 

altsGFOld = show . prAlts
altsGFRes xs = case (unStr xs) of
  [x] ->  x
  ys -> "variants"++" {" ++ unwords (intersperse ";" ys) ++ "}"

type TagId = String
type XML   = String
type Struct = Bool

string :: String -> [XML]
string = (:[])

render :: [XML] -> String
render xs = unlines xs

tag :: TagId -> [XML] -> [XML]
tag t xs = (("<" ++ t ++ ">"): (map (' ':) xs)) ++ ["</" ++ t ++ ">"]

tagA :: TagId -> (String,String) -> [XML] -> [XML]
tagA t (a,b) xs = (("<" ++ t ++ " " ++ a ++ "=\"" ++ b ++ "\"" ++ ">"): (map (' ':) xs)) ++ ["</" ++ t ++ ">"]

tagAL :: TagId -> [(String,String)] -> [XML] -> [XML]
tagAL t vk xs = (("<" ++ t ++ " " ++ (unwords [a ++ "=\"" ++ b ++ "\"" | (a,b) <- vk]) ++ ">"): (map (' ':) xs)) ++ ["</" ++ t ++ ">"]

tagA1 :: TagId -> (String,String) -> XML
tagA1 t (a,b) = "<" ++ t ++ " " ++ a ++ "=\"" ++ b ++ "\"" ++ " />"

-- | Generate XML source code.
prXML :: Dictionary -> String
prXML d =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ (render (tag "dictionary" (concat (map prEntry (unDict d)))))
 where
 prEntry  (id,stem,para,cat,inhs,tbl,_) = tagAL "entry" [("head",stem), ("paradigm",para),("inhs",unwords inhs),("xml:id",id)] (prTabl tbl)
 prTabl tbl = tag "table" $ 
	      concat [tagAL "form" [("param",a),("attr",show n)] (map (\s -> tagA1 "variant" ("val",s)) (unStr b)) | (a,(n,b)) <- tbl,
                                                                                                                                not (null (unStr b))]
-- | Generate XML source code.
prLMF :: String -> Dictionary -> String
prLMF l d =  unlines [
            "<LexicalResource dtdVersion=\"15\">",
            "<GlobalInformation>",
            "<feat att=\"languageCoding\" val=\"UTF-8\"/>", 
            "</GlobalInformation>", 
            "<Lexicon>", 
            "<feat att=\"language\" val=\"" ++ l ++ "\"/>", 
            unlines (concat (map pr (unDict d))),
            "</Lexicon>",
            "</LexicalResource>"]
 where
 pr (id,stem,para,cat,inhs,tbl,extr) =  ["<LexicalEntry>",
                                      "<feat att=\"POS\" val=\"" ++ cat ++ "\"/>",
                                      "<Lemma>",
                                      "<feat att=\"writtenForm\" val=\"" ++ stem ++ "\"/>",
                                      "</Lemma>",
                                      "<feat att=\"inherents\" val=\"" ++ (unwords inhs) ++ "\"/>"]
                                      ++
                                      (concat 
                                     [["<WordForm>",
                                      "<feat att=\"writtenForm\" val=\"" ++ w ++ "\"/>",
                                      "<feat att=\"analysis\" val=\"" ++ t ++ "\"/>",
                                      "</WordForm>"] | (w,t) <- wfs tbl])
                                     ++
                                     ["</LexicalEntry>"]
 wfs tbl = [(w,t) | (t,(_,str)) <- tbl, w <- unStr str]

-- | Print JSON
prJSON :: Dictionary -> String
prJSON = concat . concat . map prOne . unDict
 where prOne  (id,stem,para,cat,inhs,tbl,_) = 
           [conc [p "word" x, p "head" stem, p "pos" cat, p "param" a, pl "inhs" inhs, p "id" id, p "p" para, p "attr" (show attr)] | (a,(attr,s)) <- tbl, x <- unStr s]
       pl s []   = quote s ++ ":" ++ "[]"
       pl s xs   = quote s ++ ":[" ++ (concat (intersperse "," (map quote xs))) ++"]"
       p s1 s2 = quote s1 ++ ":" ++ quote s2
       conc xs = '{':(concat (intersperse "," xs)) ++ "}\n"
       

prCLEX :: Dictionary -> String
prCLEX = concat . concat . map prOne . unDict
 where prOne  (id,stem,para,cat,inhs,tbl,_) = 
           [x ++ ":" ++ conc [p "word" x, p "head" stem, p "pos" cat, p "param" a, pl "inhs" inhs, p "id" id, p "p" para, p "attr" (show attr)] | (a,(attr,s)) <- tbl, x <- unStr s]
       pl s []   = quote s ++ ":" ++ "[]"
       pl s xs   = quote s ++ ":[" ++ (concat (intersperse "," (map quote xs))) ++"]"
       p s1 s2 = quote s1 ++ ":" ++ quote s2
       conc xs = '{':(concat (intersperse "," xs)) ++ "}\n"

prHunDict :: Dictionary -> String 
prHunDict (Dict es) = unlines $ addcount $ concat $ map hunwords es
 where 
  addcount xs = show (length xs) : xs
  hunwords (_,stem,para,typ,inhs,infl,_) = [{- unwords [ -} stu s {-,"st:" ++ (stu stem),"po:"++ (stu typ),"is:" ++ stu (u ++ unwords inhs)] -} | (u,(_,str)) <- infl, s <- unStr str]
  stu s = [if (c == ' ') then '_' else c | c <- s]

prHunAffix :: Dictionary -> String
prHunAffix d = unlines ["# generated by Functional Morphology",
                        "SET UTF-8",
                        "TRY aerndtislogmkpbhfjuväcöåyqxzvw"]
			
-- | Print LexC source code
prLEXC :: Dictionary -> String
prLEXC = ("LEXICON Root\n" ++) . (++ "END") . unlines . map (uncurry prLEXCRules) . classifyDict

prLEXCRules :: Ident -> [Entry] -> String
prLEXCRules cat entries = unlines $
    ("\n! category " ++ cat ++ "\n") : (map (prEntry . noAttr) entries)
  where
    prEntry (stem,_,inhs,tbl) =
      concat (map (prForm stem inhs) ([(a,unStr b) | (a,b) <- existingForms tbl]))
    prForm stem inhs (a,b) = 
      concat [x ++ ":" ++ stem ++ prTags (a:inhs) ++ " # ;\n" | x <- b]
    prTags ts = 
      concat ["+" ++ w | t <- ts, w <- words (prFlat t)]
    altsLEXC cs = 
      unwords $ intersperse " # ;" [ s  | s <- cs] 

-- code for Xerox Finite State Tool

-- | Print XFST source code
prXFST :: Dictionary -> String
prXFST = unlines . map (uncurry prXFSTRules) . classifyDict

prXFSTRules :: Ident -> [Entry] -> String
prXFSTRules cat entries = unlines $
    ("define " ++ cat ++ " [") :
    intersperse "  |" (map (prEntry . noAttr) entries) ++
    ["  ] ;"]
  where
    prEntry (stem,_,inhs,tbl) =
      concat (intersperse "  |\n" (map (prForm stem inhs) 
				   ([(a,unStr b) | (a,b) <- existingForms tbl])))
    prForm stem inhs (a,b) = 
      "  [ " ++ 
             altsXFST b ++ " .x. " ++ 
             "{" ++ stem ++ "}" ++ prTags (a:inhs) ++ " ]"
    prTags ts = 
      unwords [" %+" ++ w | t <- ts, w <- words (prFlat t)]
    altsXFST cs = 
      unwords $ intersperse "|" ["{" ++ s ++ "}" | s <- cs] 


-- | Print SFST head source code
prSFSTHEAD :: Dictionary -> String
prSFSTHEAD d =  "ALPHABET = " ++ alphabet ++ "\n\"input.lex\""
 where alphabet :: String
       alphabet = concat $ Set.toList $ Set.fromList (concat [ tag pos : (collect (map noAttr xs))  | (pos,xs) <- classifyDict d])
       collect [] = []
       collect ((_,_,inhs,tbl):es) = map tag (inhs ++ (concat (map (words . prFlat . fst) tbl))) ++ collect es
       tag s = "<" ++ s ++ ">"

-- | Print SFST lex source code
prSFSTLEX :: Dictionary -> String
prSFSTLEX d = case classifyDict d of
            xs -> unlines (map (uncurry prSFSTLEXRules) xs)

prSFSTLEXRules :: Ident -> [Entry] -> String
prSFSTLEXRules cat entries = 
     (concat (map (prEntry . noAttr) entries)) 
  where
    prEntry (stem,_,inhs,tbl) =
      unlines (concat (map (prForm stem inhs) 
		      ([(a,unStr b) | (a,b) <- existingForms tbl])))
    prForm stem inhs (a,b) = 
       [crossproduct  (stem ++  prTags (inhs ++[a])) c | c <- b] 
    prTags ts = "<" ++ cat ++">" ++ (concat ["<" ++ w ++ ">" | t <- ts, w <- words (prFlat t)])
    zip_str [] s  = concat ["<>:" ++ pr c | c <- s]
    zip_str s  [] = concat [pr c ++ ":<>" | c <- s]
    zip_str [c1] [c2] = pr c1 ++ ":" ++ pr c2
    zip_str (c1:s1) (c2:s2)
     | c1 == c2  = c1 ++ zip_str s1 s2
     | otherwise = c1 ++ ":" ++ c2 ++ zip_str s1 s2 
    pr s
     | and (map isSpace s) = "<>"
     | otherwise           = s
    crossproduct s1 s2 = zip_str (split s1) (split s2)
    split [] = []
    split ('<':xs) = case span (/='>') xs of
                      (t,(_:xs)) -> (('<':t) ++">") : split xs
    split (':':cs) = "\\:" : split cs
    split (c:cs) = [c] : split cs
                
-- | Print SFST source code
prSFST :: Dictionary -> String
prSFST d = case classifyDict d of
            xs -> unlines (map (uncurry prSFSTRules) xs) ++ prAuto (map fst xs)
   where prAuto cs = "\n\n" ++ (unwords (intersperse "|" ["$" ++ c ++ "$" | c <- cs]))

prSFSTRules :: Ident -> [Entry] -> String
prSFSTRules cat entries = 
    ("$" ++ cat ++ "$ =\\\n") ++
     (concat (intersperse " |\\\n" (map (prEntry . noAttr) entries))) 
  where
    prEntry (stem,_,inhs,tbl) =
      concat (intersperse " |\\\n" (concat (map (prForm stem inhs) 
			       ([(a,unStr b) | (a,b) <- existingForms tbl]))))
    prForm stem inhs (a,b) = 
       ["{" ++ (fix_word stem) ++ "}:{" ++ (fix_word c)  ++ "}" ++ prTags (inhs ++[a]) | c <- b] 
    prTags ts = 
      " <" ++ cat ++">:<> " ++ (unwords ["<" ++ w ++ ">:<>" | t <- ts, w <- words (prFlat t)])
    fix_word s = concat $ map f s
    f c = case c of
           '-' -> "\\-"
           '|' -> "\\|"
           ' ' -> "\\ "
           '.' -> "\\."
           ':' -> "\\:"
           '$' -> "\\$"
           '^' -> "\\^"
           '&' -> "\\&"
           '!' -> "\\!"
           '*' -> "\\*"
           '+' -> "\\+"
           '_' -> "\\_"
           '<' -> "\\<"
           '>' -> "\\>"
           '=' -> "\\="
           '\\' -> "\\\\"
           c   -> [c]

-- | Print latex tables
prLatex :: Dictionary -> String
prLatex d = unlines (beginLatex ++ (map prLatexTable (unDict d)) ++ endLatex) where
  beginLatex = ["\\documentclass[10pt,twocolumn]{article}",
                "\\usepackage[utf8]{inputenc}",
                "\\usepackage[T1]{fontenc}",
                 "\\begin{document}"]
  endLatex   = ["\\end{document}"]

prLatexTable :: Entry -> String
prLatexTable (id,stem,para,cat,inhs,tbl,extr) = unlines
  ["\\begin{center}\n\\begin{tabular}{|l|l|}\\hline",
  "\\multicolumn{2}{|c|}{\\textbf{" ++ (esc para) ++ "}} \\\\", 
   "\\hline",
   "\\textbf{base} & " ++ (esc stem) ++ "\\\\",
   "\\hline",
   "\\textbf{pos}  & " ++ (esc cat) ++ "\\\\",
   "\\hline",
   "\\textbf{inhs} & " ++ (esc (unwords inhs)) ++ "\\\\",
  "\\hline",
  "\\textbf{inflection table} & \\\\", 
   "\\hline",
  unlines [(esc a) ++ " & " ++ pr_alts (unStr b) ++ "\n\\hline" | (a,(_,b)) <- tbl, not (null (unStr b))],
  "\\end{tabular}\n\\end{center}\n\n\\vspace{0.1cm}\n"]
 where esc [] = []
       esc ('_':xs) = "\\_" ++ esc xs
       esc (x:xs)   = x:esc xs
       pr_alts []     = "* \\\\"
       pr_alts [x]    = esc x ++ " \\\\"
       pr_alts (x:xs) = esc x ++ concat [" \\\\ \n & " ++ esc y | y <- xs] ++ " \\\\ "

wordLength = 50 :: Int
attrLength = 30 :: Int

type Schema       = String -- The database structure
type Element      = String -- the database content
type TableS       = String -- a table
type Column       = String -- a column (attribute)
type Value        = String -- a value of a column (attribute)
type DatabaseName = String 

prSqlSchema :: Dictionary-> DatabaseName -> String
prSqlSchema dict dname = 
                    "\n-- The Morphology Schema.\n\n" ++
		    "DROP DATABASE IF EXISTS " ++ dname ++ ";\n" ++
		    "CREATE DATABASE " ++  dname ++ ";\n" ++
		    "USE " ++ dname ++ ";\n\n" ++
		    lexicon ++
		    "GRANT ALL PRIVILEGES ON " ++ dname ++".* TO PUBLIC ; \n\n"
 
-- A instance needs to:
-- * Be put in the lexicon with a unique identifier
-- * Be put in the class schema
-- * Be put in the inherent schema

-- | Print SQL Code
prSQL :: Dictionary -> String
prSQL = (lexicon ++) . concat . map prSql . unDict
 where
  prSql (i,stem, para, cat, inh, t, _) = lexic i stem cat (expand t) para inh 
  lexic i stem cat t para inh = 
                 unlines [insert "LEXICON" [i,para,stem,cat,b,a,prInhs inh] | (a,b) <- t]
  prInhs [] = "-"
  prInhs xs = unwords xs
  expand t = [(a,s) | (a,(_,b)) <- t, s <- unStr b]

{-
prWordsCl ::  [(String,[((Int,String),[String])])] -> [String]
prWordsCl                  [] = []
prWordsCl ((c,((n1,w1),as1):xs):xss)
    = (insert c ([show n1,w1,show n1] ++ as1) :
       [insert c ([show n,w,show n1] ++as) | ((n,w),as) <- xs]) ++
       prWordsCl xss
    
innerNumber :: [(a,[(b,[c])])] -> Int -> [(a,[((Int,b),[c])])]
innerNumber [] _ = []
innerNumber ((a,xs):xss) n = (a,number xs n) : 
			     innerNumber xss (n+(length xs))
 where number xs n = zipWith f [n..] xs
       f n (s,zs) = ((n,s),zs)
-}

-----------------------------------------------------

emptyE :: Value
emptyE = "NULL"

insert :: TableS -> [Value] -> Element
insert t vs = "INSERT INTO " ++ t ++ " VALUES ('" 
	      ++ (concat (intersperse "','" vs)) ++"');"

type Name           = String
type Type           = String
type TypeConstraint = String
type Constraint     = String

primaryKey :: Name -> Constraint
primaryKey n = "PRIMARY KEY (" ++ n ++ ")"

foreignKey :: Name -> (Name,Name) -> Constraint
foreignKey n (n1,n2) = "FOREIGN (" ++ n ++ ") REFERENCES " ++
		       n1 ++ "(" ++ n2 ++ ")"

varchar :: Int -> Type
varchar n = "VARCHAR(" ++ show n ++ ")"

intType :: Type
intType = "INTEGER"

notNull :: TypeConstraint
notNull = "NOT NULL"

createTable :: Name -> [(Name,Type,TypeConstraint)] -> [Constraint] -> TableS
createTable n xs cs =
    "CREATE TABLE " ++ n ++ "\n(\n" ++
    (concat ((intersperse ",\n" [n ++ " " ++ t ++ " " ++ tc | (n,t,tc) <- xs])))
    ++ concat (intersperse ",\n" cs) ++ ");\n\n"

lexicon :: TableS
lexicon = createTable "LEXICON" 
	  [
	   ("ID", varchar wordLength, notNull),
	   ("PID", varchar wordLength, notNull),
	   ("HEAD",varchar wordLength,notNull),
	   ("POS",varchar wordLength,notNull),
	   ("WORD",varchar wordLength,notNull),
	   ("PARAM",varchar wordLength,notNull),
	   ("INHS",varchar wordLength,notNull)
          ] []

