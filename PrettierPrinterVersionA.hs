module PrettierPrinter
  (
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

best :: Int -> Int -> Doc -> Doc
best width charsUsed Nil               = Nil
best width charsUsed (Text str doc)    = Text str (best width (charsUsed + length str) doc)
best width charsUsed (Line n doc)      = Line n (best width (width - n) doc)
best width charsUsed (Union doc1 doc2) = better width charsUsed
                                                (best width charsUsed doc1)
                                                (best width charsUsed doc2)
                                                
better :: Int -> Int -> Doc -> Doc -> Doc
better width charsUsed doc1 doc2 = if fits (width - charsUsed) doc1 then doc1 else doc2

fits :: Int -> Doc -> Bool
fits charsLeft _ | charsLeft < 0 = False
fits _ Nil                       = True
fits _ (Line _ doc)              = True
fits charsLeft (Text str doc)    = fits (charsLeft - length str) doc

pretty :: Int -> Doc -> Doc
pretty width doc = best width 0 doc
