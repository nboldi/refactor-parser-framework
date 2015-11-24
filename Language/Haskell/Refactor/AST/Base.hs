module Language.Haskell.Refactor.AST.Base where

import SourceCode.ASTElems

data QName a = QName { qualifiers :: ASTList Name a
                     , unqualifiedName :: Name a 
                     , qnameInfo :: a
                     }
  
-- | Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data Name a = Name { nameStr :: String
                   , nameInfo :: a
                   } deriving Show
data DataOrNewKeyword a
  = DataKeyword { dataKeywordInfo :: a }
  | NewdataKeyword { dataKeywordInfo :: a }
    
data DoKind a
  = DoKeyword { doKeywordInfo :: a }
  | MDoKeyword { doKeywordInfo :: a }
  
data TypeKeyword a
  = TypeKeyword { typeKeywordInfo :: a }
  
data CallConv a
  = StdCall    { callConvInfo :: a }
  | CCall      { callConvInfo :: a }
  | CPlusPlus  { callConvInfo :: a }
  | DotNet     { callConvInfo :: a }
  | Jvm        { callConvInfo :: a }
  | Js         { callConvInfo :: a }
  | JavaScript { callConvInfo :: a }
  | CApi       { callConvInfo :: a }
  
data ArrowAppl a
  = LeftAppl     { arrowApplInfo :: a }
  | RightAppl    { arrowApplInfo :: a }
  | LeftHighApp  { arrowApplInfo :: a }
  | RightHighApp { arrowApplInfo :: a }
  
data Safety a
  = Safe          { safetyKeywordInfo :: a }
  | ThreadSafe    { safetyKeywordInfo :: a }
  | Unsafe        { safetyKeywordInfo :: a }
  | Interruptible { safetyKeywordInfo :: a }

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
               
data Literal a
  = CharLit       { charLitValue :: Char 
                  , literalInfo :: a
                  }
  | StringLit     { stringLitValue :: String 
                  , literalInfo :: a
                  }
  | IntLit        { intLitValue :: Integer 
                  , literalInfo :: a
                  }
  | FracLit       { fracLitValue :: Rational
                  , literalInfo :: a
                  }
  | PrimIntLit    { intLitValue :: Integer
                  , literalInfo :: a
                  }
  | PrimFloatLit  { floatLitValue :: Rational
                  , literalInfo :: a
                  }
  | PrimDoubleLit { floatLitValue :: Rational
                  , literalInfo :: a
                  }
  | PrimCharLit   { charLitValue :: Char 
                  , literalInfo :: a
                  }
  | PrimStringLit { stringLitValue :: String 
                  , literalInfo :: a
                  }
               
data Promoted a
  = PromotedInt    { promotedIntValue :: Integer
                   , promotedInfo :: a
                   }
  | PromotedString { promotedStringValue :: String
                   , promotedInfo :: a
                   }
  | PromotedCon    { promotedConName :: Name a
                   , promotedInfo :: a
                   }
  | PromotedList   { promotedElements :: ASTList Promoted a
                   , promotedInfo :: a
                   }
  | PromotedTuple  { promotedElements :: ASTList Promoted a
                   , promotedInfo :: a
                   }
  | PromotedUnit   { promotedInfo :: a }
               