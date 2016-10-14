module DotGraphTypes where

import           Data.List
import qualified Data.Text as T

data DotGraph = DotGraph
  { dgName       :: !T.Text
  , dgLabel      :: !T.Text
  , dgStatements :: [DotStatement]
  } deriving (Eq, Show)

data DotStatement = SG DotSubGraph | DN DotNode | DE DotEdge
  deriving (Eq, Show)

data DotSubGraph = DotSubGraph
  { dsgName       :: !T.Text
  , dsgLabel      :: !T.Text
  , dsgStatements :: [DotStatement]
  } deriving (Eq, Show)

data DotNode = DotNode
  { dnName  :: !T.Text
  , dnLabel :: !T.Text
  } deriving (Eq, Show)

findNode :: T.Text -> [DotNode] -> Maybe DotNode
findNode name = find $ \x -> dnName x == name

data DotEdge = DotEdge
  { deFrom  :: DotNode
  , deTo    :: DotNode
  , deLabel :: !T.Text
  } deriving (Eq, Show)

data DotAttribute = DotAttribute
  { daName  :: !T.Text
  , daValue :: DotValue
  } deriving (Eq, Show)

findAttribute :: T.Text -> [DotAttribute] -> Maybe DotAttribute
findAttribute name = find p
  where p (DotAttribute n _) = if n == name then True else False

data DotValue = StringVal !T.Text | IdentVal !T.Text deriving Eq

instance Show DotValue where
  show (StringVal t) = T.unpack t
  show (IdentVal t)  = T.unpack t
