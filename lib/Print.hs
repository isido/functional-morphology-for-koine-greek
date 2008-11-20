module Print where

import General
import Dictionary
import List (intersperse)
import IO
import UTF8

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
prDictionary = unlines . map (unlines . prOne) . removeAttr where
  prOne (stem, typ, inhs, infl) = stem : typ : unwords inhs : 
                             [a ++ ": " ++ prStr s | (a,s) <- infl]

-- | Print Dictionary in a structured format.
prNewDictionary :: Dictionary -> String
prNewDictionary = unlines . map prOne . unDict where
  prOne (stem, para, typ, inhs, infl,ex) = 
   concat ["{",
           prId (stem++"_"++typ),
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

-- | Write a fullform lexicon to handle.
prFullFormLex :: Handle -> FullFormLex -> IO()
prFullFormLex _ [] = return ()
prFullFormLex h (x:xs) = do prOne x
                            prFullFormLex h xs
 where
  prOne (s,ps) = sequence_ [hPutStr h (encodeUTF8 s) >> a | a <- map prAttr ps]
  prAttr (a,ss) = hPutStr h (encodeUTF8 (':':ss)) >> prCompAttr h a


-- | Print attribute to handle.
prCompAttr :: Handle -> Attr -> IO()
prCompAttr h a = hPutStrLn h $ encodeUTF8 $ " [" ++ show a ++ "] "

-- |  Generate GF paradigm functions.
prGFRes :: Dictionary -> String
prGFRes dict = (unlines (map prGFOper (zip [0..] (removeAttr dict))))

prGFOper :: (Int,(String, Ident, [Ident], Table Ident)) -> String
prGFOper (i,(oper, ty, inhs, tab0)) = begin ++ " : Str -> " ++ ty ++ " = " ++ bind ++ rec ++ end where 
  begin = "oper " ++ (oper ++ "_" ++ show i) -- Reduce the number of name clashes! 
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


-- | Print GF source code.
prGF :: Dictionary -> String
prGF dict = cats ++ (unlines (map prGFRule (zip [0..] (removeAttr dict))))
 where cs = unlines ["cat " ++ c ++ ";" | c <- map fst $ classifyDict dict]
       cats = "\n" ++ cs ++ "\n\n"
prGFRule ::  (Int,(String, Ident, [Ident], Table Ident)) -> String
prGFRule (i,(id,cat,inhs,tab)) = let name = id ++ "_" ++ show i in
  "fun " ++ name ++ " : " ++ cat ++ " ;\n\n" ++
  "lin " ++ name ++ " = {s = table {\n" ++ 
    concat (intersperse " ;\n" 
               ["  "++ a ++ " => "++ altsGF b | (a,b) <- tab]) ++
          (if null inhs then "}" else " };\n  ") ++
          concat (intersperse " ;\n  " 
                    ["h" ++ show i ++ " = " ++ p | (i,p) <- zip [1..] inhs]
                 ) ++
     "\n} ;\n"

-- | two GF modes for free variation; old for GF<0.98 
altsGF xs = case (unStr xs) of
  [x] -> prQ x
  ys -> "variants"++" {" ++ unwords (intersperse ";" (map prQ ys)) ++ "}" where
 where
   prQ s = '"' : s ++ "\""
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

tagA1 :: TagId -> (String,String) -> XML
tagA1 t (a,b) = "<" ++ t ++ " " ++ a ++ "=\"" ++ b ++ "\"" ++ " />"


-- | Generate XML source code.
prXML :: Dictionary -> String
prXML d =  "<?xml version=\"1.0\"?>\n" ++ (render (tag "lexicon" (concat (map (uncurry pr) (classifyDict d)))))
 where
 pr cat entries = tagA "class" ("category",cat) (concat (map (prEntry . noAttr) entries))
 prEntry (stem,_,inhs,tbl) = tag "lexicon_entry" $ tagA1 "dictionary_form" ("value",stem) :(prInh inhs ++ prTabl tbl)
 prInh inhs = map (\s -> tagA1 "inherent" ("value",s)) inhs
 prTabl tbl = tag "inflection_table" $ 
	      concat [tagA "inflection_form" ("pos",a) (map (\s -> tagA1 "variant" ("word",s)) (unStr b)) | (a,b) <- existingForms tbl]
				
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
      "  [ {" ++ stem ++ "}" ++ prTags (a:inhs) ++ " .x. " ++ altsXFST b ++"]"
    prTags ts = 
      unwords [" %+" ++ w | t <- ts, w <- words (prFlat t)]
    altsXFST cs = 
      unwords $ intersperse "|" ["{" ++ s ++ "}" | s <- cs] 

-- | Print latex tables
prLatex :: Dictionary -> String
prLatex d = unlines (beginLatex ++ map prLatexTable (removeAttr d) ++ endLatex) where
  beginLatex = ["\\documentclass{report}",
                "\\usepackage[latin1]{inputenc}",
                "\\begin{document}"]
  endLatex   = ["\\end{document}"]

prLatexTable :: EntryN -> String
prLatexTable (ident,cat,inhs,tab) =
  unwords ((ident ++ ",") : cat : inhs) ++ "\n" ++ 
  "\\begin{center}\n\\begin{tabular}{|l|l|}\\hline\n" ++
  unlines [a ++ " & {\\em " ++ prAlts b ++ "} \\\\" | (a,b) <- tab] ++
  "\\hline\n\\end{tabular}\n\\end{center}\n\\newpage\n\n"

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
prSQL = (lexicon ++) . unlines . map prSql . zip [1..] . removeAttr 
 where
  prSql (i,(stem, cat, inh, table)) = lexic i stem  cat (expand table inh) 
  lexic i stem cat t = 
                 unlines [insert "LEXICON" [show i,stem,cat,b,a] | (a,b) <- t]
  expand table inh = [(a ++ " - " ++ (unwords inh) ,s) | (a,b) <- table,
			                                         s <- unStr b]

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
	   ("ID", intType, notNull),
	   ("DICTIONARY",varchar wordLength,notNull),
	   ("CLASS",varchar wordLength,notNull),
	   ("WORD",varchar wordLength,notNull),
	   ("POS",varchar wordLength,notNull)
          ] []



