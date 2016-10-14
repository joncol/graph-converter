{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DotGraphLexer where

import           Text.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok

langDef :: LanguageDef st
langDef = emptyDef
  { Tok.commentLine     = "//"
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.reservedNames   = [ "digraph"
                          , "id"
                          , "label"
                          , "ltail"
                          , "shape"
                          , "style"
                          , "subgraph "
                          ]
  , Tok.reservedOpNames = [ "="
                          , "->"
                          ]
  }

lexer = Tok.makeTokenParser langDef

reservedNames = Tok.reservedNames langDef
identifier    = Tok.identifier    lexer
reserved      = Tok.reserved      lexer
reservedOp    = Tok.reservedOp    lexer
braces        = Tok.braces        lexer
brackets      = Tok.brackets      lexer
stringLiteral = Tok.stringLiteral lexer
ws            = Tok.whiteSpace
