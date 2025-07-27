module L.L2.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the syntax of L2
-- programs
-- each L2 program is just a sequence of
-- statements

data L2
  = L2 [S2]
  deriving (Eq, Ord, Show)

-- statements can be a read, print or
-- an assignment.

data S2
  = Def Var E2 [S2]
  | LRead String Var
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

-- expressions

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)

instance Pretty L2 where
  ppr (L2 ss) =
    -- para cada S2, pôr um ";" à direita e depois empilhar com vcat
    vcat (punctuate (text ";") (map ppr ss))

instance Pretty S2 where
  ppr (Def v e ss) =
    let
      -- cabeçalho
      header =
        hsep
          [ text "def"
          , ppr v
          , text ":="
          , ppr e
          , text "in"
          ]
      -- corpo: cada declaração interna, já com seu próprio ppr e ";" via L2 acima
      bodyDocs = map ppr ss
      -- empilha e pontua com ";" cada S2 interno
      body = nest 2 (vcat (punctuate (text ";") bodyDocs))
      footer = text "end"
     in
      -- pune cada parte em linhas separadas
      vcat [header, body, footer]
  ppr (LRead s v) =
    hsep
      [ text "read("
      , doubleQuotes (text s)
      , comma
      , ppr v
      , text ")"
      ]
  ppr (LPrint e) =
    hsep [text "print(", ppr e, text ")"]
  ppr (LAssign v e) =
    hsep [ppr v, text ":=", ppr e]

instance Pretty E2 where
  ppr = pprAdd

pprAdd :: E2 -> Doc
pprAdd (LAdd x y) = hsep [pprAdd x, text "+", pprAdd y]
pprAdd other = pprMinus other

pprMinus :: E2 -> Doc
pprMinus (LMinus x y) = hsep [pprMinus x, text "-", pprMinus y]
pprMinus other = pprMul other

pprMul :: E2 -> Doc
pprMul (LMul x y) = hsep [pprMul x, text "*", pprMul y]
pprMul other = pprDiv other

pprDiv :: E2 -> Doc
pprDiv (LDiv x y) = hsep [pprDiv x, text "/", pprFact y]
pprDiv other = pprFact other

pprFact :: E2 -> Doc
pprFact (LVal v) = ppr v
pprFact (LVar v) = ppr v
pprFact other = parens (ppr other)
