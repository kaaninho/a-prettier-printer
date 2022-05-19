module PrettierPrinter
  ( flatten
  , nest
  , nil
  , (<>)
  )
where

import Prelude hiding ((<>))

infixr 5 :<|>
infixr 6 :<>
infixr 6 <>

data DOC
  = NIL
  | DOC :<> DOC
  | NEST Int DOC
  | TEXT String
  | LINE
  | DOC :<|> DOC
  deriving (Show)

data Doc
  = Nil
  | Text String Doc
  | Line Int Doc
  deriving (Show)


nil       = NIL
x <> y    = x :<> y
nest i x  = NEST i x
text s    = TEXT s
line      = LINE

group x   = flatten x :<|> x


flatten :: DOC -> DOC
flatten NIL         = NIL
flatten (x :<> y)   = flatten x :<> flatten y
flatten (NEST i x)  = NEST i (flatten x)
flatten (TEXT s)    = TEXT s
flatten LINE        = TEXT " "
flatten (x :<|> _)  = flatten x

layout :: Doc -> String
layout Nil         = ""
layout (Text s x)  = s ++ layout x
layout (Line i x)  = "\n" ++ (take i $ repeat ' ') ++ layout x


best :: Int -> Int -> DOC -> Doc
best w k x = be w k [(0, x)]


be :: Int -> Int -> [(Int, DOC)] -> Doc
be w k []                 = Nil
be w k ((i, NIL):z)       = be w k z
be w k ((i, x :<> y):z)   = be w k ((i,x):(i,y):z)
be w k ((i, NEST j x):z)  = be w k ((i+j,x):z)
be w k ((i, TEXT s):z)    = Text s (be w (k + length s) z)
be w k ((i, LINE):z)      = Line i (be w i z)
be w k ((i, x :<|> y):z)  = better w k (be w k ((i, x):z))
                                       (be w k ((i, y):z))

better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y = if fits (w-k) x then x else y

fits :: Int -> Doc -> Bool
fits w x | w < 0   = False
fits _ Nil         = True
fits _ (Line i x)  = True
fits w (Text s x)  = fits (w - length s) x



pretty :: Int -> DOC -> String
pretty w x = layout (best w 0 x)


-- Utility functions

(<+>) :: DOC -> DOC -> DOC
x <+> y = x <> text " " <> y

(</>) :: DOC -> DOC -> DOC
x </> y = x <> line <> y


folddoc :: (DOC -> DOC -> DOC) -> [DOC] -> DOC
folddoc _ []      = nil
folddoc _ [x]     = x
folddoc f (x:xs)  = f x (folddoc f xs)

spread :: [DOC] -> DOC
spread = folddoc (<+>)

stack :: [DOC] -> DOC
stack = folddoc (</>)

bracket :: String -> DOC -> String -> DOC
bracket l x r = group (text l <>
                  nest 2 (line <> x) <>
                  line <> text r)

(<+/>) :: DOC -> DOC -> DOC
x <+/> y = group (x </> y)

fillwords :: String -> DOC
fillwords = folddoc (<+/>) . map text . words

fill :: [DOC] -> DOC
fill []        = nil
fill [x]       = x
fill (x:y:zs)  = (flatten x <+> fill (flatten y : zs))
                 :<|>
                 (x </> fill (y : zs))


-- Eigenes XML-Beispiel

data XML = Element String [Attribute] [XML]
         | Txt String

data Attribute = Attribute String String

concatDOCs :: [DOC] -> DOC
concatDOCs []      = nil
concatDOCs [x]     = x
concatDOCs (x:xs)  = x </> concatDOCs xs


attToDOC :: Attribute -> DOC
attToDOC (Attribute key value) = text key <> text "=\"" <> text value <> text "\""

tagToDOC :: String -> [Attribute] -> DOC
tagToDOC name [] = text "<" <> text name <> text ">"
tagToDOC name attributes = group
                           (text "<" <> text name <>
                            (nest 2 (line <> concatDOCs (map attToDOC attributes))) <>
                            line <> text ">")



xmlToDOC :: XML -> DOC
xmlToDOC (Txt string)                    = text string
xmlToDOC (Element name attributes xmls)  = group
                                           (tagToDOC name attributes </>
                                            nest 2 (group line <>
                                                    concatDOCs (map xmlToDOC xmls)) </>
                                            text "</" <> text name <> text ">")


-- XML
xml   = Element "p" [
          Attribute "color" "red",
          Attribute "font" "Times",
          Attribute "size" "10"
        ] [
          Txt "Here is some",
          Element "em" [] [Txt "emphasized"],
          Txt "Text. Here is a",
          Element "a" [Attribute "href" "http://example.org"]
            [Txt "link"],
          Txt "elsewhere."
          ]

-- XML example from Paper
-- showXML :: XML -> DOC
-- showXML x  = folddoc (<>) (showXMLs x)

-- showXMLs :: XML -> [DOC]
-- showXMLs (Elt n a [])  = [text "<" <> showTag n a <> text "/>"]
-- showXMLs (Elt n a c)   = [text "<" <> showTag n a <> text ">" <>
--                           showFill showXMLs c <>
--                           text "</" <> text n <> text ">"]
-- showXMLs (Txt s)       = map text (words s)

-- showAtt :: Att -> [DOC]
-- showAtt (Att k v) = [text k <> text "=" <> text (quoted v)]

-- quoted v = "\"" ++ v ++ "\""

-- showTag :: String -> [Att] -> DOC
-- showTag n a = text n <> showFill showAtt a

-- showFill :: (a -> [DOC]) -> [a] -> DOC
-- showFill f [] = nil
-- showFill f xs = bracket "" (fill (concat (map f xs))) ""
