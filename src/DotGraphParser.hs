{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DotGraphParser where

import           Control.Monad.Identity
import qualified Data.Text as T
import           Text.Parsec hiding (label)

import           DotGraphTypes
import           DotGraphIntermediateTypes
import           DotGraphLexer

data ParserState = ParserState
  { psNodes :: [DotNode]
  }

type Parser a = ParsecT String ParserState Identity a

initialState :: ParserState
initialState = ParserState
  { psNodes = []
  }

parseDotgraph :: Parser IntermediateDotGraph
parseDotgraph = flip (<?>) "Dotgraph" $
  do
    _     <- reserved "digraph"
    name  <- stringLiteral
    braces $ do
      lbl   <- label
      _     <- many skipAttribute
      stmts <- many statement

      return IntermediateDotGraph
        { idgName       = T.pack name
        , idgLabel      = lbl
        , idgStatements = stmts
        }

attribute :: Parser DotAttribute
attribute = flip (<?>) "Attribute" $
  do
    spaces
    name <- choice $ map (\n -> reserved n >> return n) reservedNames
    reservedOp "="
    val <- StringVal . T.pack <$> stringLiteral <|>
           IdentVal  . T.pack <$> identifier
    return DotAttribute
      { daName  = T.pack name
      , daValue = val
      }

label :: Parser T.Text
label = flip (<?>) "Label" $
  do
    spaces
    _   <- reserved "label"
    _   <- reservedOp "="
    lbl <- stringLiteral
    return $ T.pack lbl

skipAttribute :: Parser ()
skipAttribute = try $
  do
    _ <- string "graph" <|> string "node" <|> string "edge"
    _ <- manyTill anyChar endOfLine
    spaces
    return ()

statement :: Parser IntermediateDotStatement
statement = ISG <$> subGraph <|>
            IDN <$> node <|>
            IDE <$> edge

subGraph :: Parser IntermediateDotSubGraph
subGraph = try $
  do
    spaces
    _     <- reserved "subgraph"
    name  <- identifier
    braces $ do
      lbl   <- label
      stmts <- many statement

      return IntermediateDotSubGraph
        { idsgName       = T.pack name
        , idsgLabel      = lbl
        , idsgStatements = stmts
        }

node :: Parser DotNode
node = try $
  do
    name  <- identifier
    attrs <- brackets $ many attribute
    let la = findAttribute "label" attrs
    let n = DotNode { dnName  = T.pack name
                    , dnLabel = maybe "" (T.pack . show . daValue) la
                    }
    modifyState (addNode n)
    return n
  where addNode n (s@ParserState { psNodes }) = s { psNodes = n : psNodes }

edge :: Parser IntermediateDotEdge
edge = try $
  do
    from  <- identifier
    _     <- reservedOp "->"
    to    <- identifier
    attrs <- option [] $ brackets (many attribute)
    let la = findAttribute "label" attrs
    return IntermediateDotEdge
      { ideFrom  = T.pack from
      , ideTo    = T.pack to
      , ideLabel = maybe "" (T.pack . show . daValue) la
      }
