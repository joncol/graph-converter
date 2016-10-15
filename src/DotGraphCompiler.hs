{-# LANGUAGE OverloadedStrings #-}

module DotGraphCompiler where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Text.PrettyPrint.Leijen as PP
import           Text.Parsec

import           DotGraphIntermediateTypes
import           DotGraphParser
import           DotGraphTypes
import           PlantUMLPrettyPrinter

compile :: BS.ByteString -> Either String String
compile bs =
  do
    case runParser ((,) <$> parseDotgraph
                        <*> getState) initialState "" (BS.unpack bs) of
      Left e       -> Left  $ "Parse error: " ++ show e
      Right (g, s) -> Right $ flip PP.displayS ""
                            $ PP.renderPretty 1 80 . PP.pretty
                            . DotGraphPP $ secondPass (psNodes s) g

secondPass :: [DotNode] -> IntermediateDotGraph -> DotGraph
secondPass ns g = DotGraph
  { dgName       = idgName g
  , dgLabel      = idgLabel g
  , dgStatements = map (transformStatement ns) $ idgStatements g
  }

-- is there a way to avoid having to define this?
unknownNode :: DotNode
unknownNode = DotNode "unknown_name" "unknown_label"

transformStatement :: [DotNode] -> IntermediateDotStatement -> DotStatement
transformStatement ns (ISG (IntermediateDotSubGraph name lbl stmts)) =
  SG $ DotSubGraph name lbl (map (transformStatement ns) stmts)
transformStatement _ (IDN n) = DN n
transformStatement ns (IDE (IntermediateDotEdge from to lbl)) =
  DE $ (DotEdge (fromMaybe unknownNode (findNode from ns))
                (fromMaybe unknownNode (findNode to ns))
                lbl)
