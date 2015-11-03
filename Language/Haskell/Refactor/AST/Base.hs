module Language.Haskell.Refactor.AST.Base where

import SourceCode.ASTElems

data QName a = QName { qualifiers :: ASTList Name a
                     , unqualifiedName :: Name a 
                     }
                     
data Name a = Name { nameStr :: String
                   , nameInfo :: a
                   }
