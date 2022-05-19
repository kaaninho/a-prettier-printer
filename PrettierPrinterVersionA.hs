module PrettierPrinter
  (
--    flatten
  nest
  , nil
  , (<>)
  )
where

import Prelude hiding ((<>))

infixr 6 <>

data Doc
  = Nil
  | Text String Doc
  | Line Int Doc
  | Union Doc Doc
  deriving (Show)

layout :: Doc -> String
layout Nil         = ""
layout (Text s x)  = s ++ layout x
layout (Line i x)  = "\n" ++ (take i $ repeat ' ') ++ layout x

------------------------------
-- Blogpost 2
------------------------------

-- Beispieldokument
doc = Text "Hallo!" $ 
      Line 0 $
      Line 0 $
      Text "Wir schreiben schöne Texte, die" $
      Line 2 $
      Text "- eingerückt sind" $
      Line 2 $ 
      Text "- und Zeilenumbrüche" $
      Line 4 $
      Text "- enthalten" $
      Nil

doc2 = Text "Hallo!" 
       (Line 0
        (Line 0
         (Text "Wir schreiben schöne Texte, die"
          (Line 2
           (Text "- eingerückt sind"
            (Line 2 
             (Text "- und Zeilenumbrüche"
              (Line 4
               (Text "- enthalten" Nil)))))))))

-- Operatoren

nil :: Doc
nil = Nil

text :: String -> Doc
text str = Text str Nil

line :: Doc
line = Line 0 Nil

nest :: Int -> Doc -> Doc
nest n Nil               = Nil
nest n (Text str doc)    = Text str (nest n doc)
nest n (Line indent doc) = Line (n + indent) (nest n doc)

(<>) :: Doc -> Doc -> Doc
Text str doc <> doc2 = Text str (doc <> doc2)
Line n doc <> doc2   = Line n (doc <> doc2)
Nil <> doc           = doc

--- Beispieldokument mit Operatoren
doc3 = text "Hallo" <> line <> line
       <> text "Wir schreiben schöne Texte, die"
       <> nest 2 (line <> (text "- eingerückt sind")
                  <> text "- und Zeilenumbrüche"
                  <> nest 2 (line <> text "- enthalten"))


-- `group` mit `flatten` und `<|>`

infixr 6 <|>

(<|>) :: Doc -> Doc -> Doc
doc1 <|> doc2 = Union doc1 doc2

flatten :: Doc -> Doc
flatten Nil = Nil
flatten (Text str doc) = Text str (flatten doc)
flatten (Line n doc) = Text " " (flatten doc)
flatten (Union doc1 doc2) = flatten doc1
       
group :: Doc -> Doc
group doc = flatten doc <|> doc


           

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
