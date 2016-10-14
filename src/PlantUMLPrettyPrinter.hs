{-# LANGUAGE OverloadedStrings #-}

module PlantUMLPrettyPrinter
  ( DotGraphPP (..)
  ) where

import           Prelude hiding ((<$>))
import qualified Data.Text as T
import           Text.PrettyPrint.Leijen

import           DotGraphTypes

text' :: T.Text -> Doc
text' = text . T.unpack . T.replace "\n" "\\n"

newtype DotGraphPP = DotGraphPP DotGraph

header :: [String]
header =
  [ "skinparam shadowing false"
  , "skinparam dpi 100"
  , "skinparam backgroundColor #F1FAEE"
  , "skinparam noteBackgroundColor #E63946"
  , "skinparam noteBorderColor #000000"
  , "skinparam state {"
  , "  BackgroundColor #A8DADC"
  , "  BorderColor #000000"
  , "  ArrowColor #1D3557"
  , "  StartColor #1D3557"
  , "}"
  , "skinparam defaultFontName Ubuntu"
  ]

instance Pretty DotGraphPP where
  pretty (DotGraphPP g) =
    text "@startuml" <$>
    vcat (map text header) <$>
    text "title" <+> dquotes (text' . dgLabel $ g) <$>
    vsep (map (pretty . DotStatementPP) $ dgStatements g) <$>
    text "@enduml" <> linebreak

newtype DotStatementPP = DotStatementPP DotStatement
instance Pretty DotStatementPP where
  pretty (DotStatementPP s) =
    case s of
      SG dsg -> pretty $ DotSubGraphPP dsg
      DN dn  -> pretty $ DotNodePP dn
      DE de  -> pretty $ DotEdgePP de

newtype DotSubGraphPP = DotSubGraphPP DotSubGraph
instance Pretty DotSubGraphPP where
  pretty (DotSubGraphPP s) =
    text "state" <+> text' (dsgLabel s) <+>
    braces (linebreak <>
            indent 2 (vcat $ map (pretty . DotStatementPP) $ dsgStatements s) <>
            linebreak)

newtype DotNodePP = DotNodePP DotNode
instance Pretty DotNodePP where
  pretty (DotNodePP (DotNode _ lbl)) = node
    where node = if (T.null lbl)
                   then empty
                   else text "state" <+> text' lbl

newtype DotEdgePP = DotEdgePP DotEdge
instance Pretty DotEdgePP where
  pretty (DotEdgePP (DotEdge from to label)) =
    text' (dnLabel from <?> "[*]") <+> text "-->" <+> text' (dnLabel to) <> lbl
    where lbl = if T.null label
                  then empty
                  else text ":" <+> text' label

(<?>) :: T.Text -> T.Text -> T.Text
s <?> t
  | T.null s  = t
  | otherwise = s

infixr 7 <?>
