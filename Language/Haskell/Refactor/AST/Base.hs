module Language.Haskell.Refactor.AST.Base where

import SourceCode.ASTElems

data QName a = QName { qualifiers :: ASTList Name a
                     , unqualifiedName :: Name a 
                     }
                     
data Name a = Name { nameStr :: String
                   , nameInfo :: a
                   }

  
data DataOrNewKeyword a
  = DataKeyword { dataKeywordInfo :: a }
  | NewdataKeyword { newdataKeywordInfo :: a }
  
             
-- | Associativity of an operator.
data Assoc a
  = AssocNone { assocInfo :: a } -- ^ non-associative operator (declared with @infix@)
  | AssocLeft { assocInfo :: a } -- ^ left-associative operator (declared with @infixl@)
  | AssocRight { assocInfo :: a } -- ^ right-associative operator (declared with @infixr@)
  
-- | Numeric precedence of an operator
data Precedence a
  = Precedence { precedenceValue :: Int
               , precedenceInfo :: a
               }