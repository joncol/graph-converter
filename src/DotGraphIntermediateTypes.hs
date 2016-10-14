module DotGraphIntermediateTypes where

import qualified Data.Text as T

import           DotGraphTypes

data IntermediateDotGraph = IntermediateDotGraph
  { idgName       :: !T.Text
  , idgLabel      :: !T.Text
  , idgStatements :: [IntermediateDotStatement]
  } deriving (Eq, Show)

data IntermediateDotStatement = ISG IntermediateDotSubGraph |
                                IDN DotNode |
                                IDE IntermediateDotEdge
  deriving (Eq, Show)

data IntermediateDotSubGraph = IntermediateDotSubGraph
  { idsgName       :: !T.Text
  , idsgLabel      :: !T.Text
  , idsgStatements :: [IntermediateDotStatement]
  } deriving (Eq, Show)

data IntermediateDotEdge = IntermediateDotEdge
  { ideFrom  :: !T.Text
  , ideTo    :: !T.Text
  , ideLabel :: !T.Text
  } deriving (Eq, Show)
