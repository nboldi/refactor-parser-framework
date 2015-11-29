module Language.Haskell.Refactor.AST 
  ( module Language.Haskell.Refactor.AST.Module
  , module Language.Haskell.Refactor.AST.Decl
  , module Language.Haskell.Refactor.AST.Base
  , SemaInfo, RI
  ) where

import Language.Haskell.Refactor.AST.Module
import Language.Haskell.Refactor.AST.Decl
import Language.Haskell.Refactor.AST.Base
import SourceCode.InfoTypes
import SourceCode.Semantics

type SemaInfo = ()

type RI = NodeInfo RangeInfo SemaInfo

