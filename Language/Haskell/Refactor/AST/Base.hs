module Language.Haskell.Refactor.AST.Base where

import SourceCode.ASTElems
  
-- | Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data Name a = Name { qualifiers      :: ASTList SimpleName a
                   , unqualifiedName :: SimpleName a 
                   , qnameInfo       :: a
                   } deriving Show
                  
data SimpleName a 
  = SimpleName { simplNameStr  :: String
               , simpleNameInfo :: a
               } deriving Show
                   
data DataOrNewtypeKeyword a
  = DataKeyword { dataKeywordInfo :: a }
  | NewtypeKeyword { dataKeywordInfo :: a }
  deriving Show
    
data DoKind a
  = DoKeyword { doKeywordInfo :: a }
  | MDoKeyword { doKeywordInfo :: a }
  deriving Show
  
data TypeKeyword a
  = TypeKeyword { typeKeywordInfo :: a }
  deriving Show
  
-- | Recognised overlaps for overlap pragmas. Can be applied to class declarations and class instance declarations.    
data OverlapPragma a
  = EnableOverlap { overlapInfo :: a } -- ^ OVERLAP pragma
  | DisableOverlap { overlapInfo :: a } -- ^ NO_OVERLAP pragma
  | Overlappable { overlapInfo :: a } -- ^ OVERLAPPABLE pragma
  | Overlapping { overlapInfo :: a } -- ^ OVERLAPPING pragma
  | Overlaps { overlapInfo :: a } -- ^ OVERLAPS pragma
  | IncoherentOverlap { overlapInfo :: a } -- ^ INCOHERENT pragma
  deriving Show
  
data CallConv a
  = StdCall    { callConvInfo :: a }
  | CCall      { callConvInfo :: a }
  | CPlusPlus  { callConvInfo :: a }
  | DotNet     { callConvInfo :: a }
  | Jvm        { callConvInfo :: a }
  | Js         { callConvInfo :: a }
  | JavaScript { callConvInfo :: a }
  | CApi       { callConvInfo :: a }
  deriving Show
  
data ArrowAppl a
  = LeftAppl     { arrowApplInfo :: a }
  | RightAppl    { arrowApplInfo :: a }
  | LeftHighApp  { arrowApplInfo :: a }
  | RightHighApp { arrowApplInfo :: a }
  deriving Show
  
data Safety a
  = Safe          { safetyKeywordInfo :: a }
  | ThreadSafe    { safetyKeywordInfo :: a }
  | Unsafe        { safetyKeywordInfo :: a }
  | Interruptible { safetyKeywordInfo :: a }
  deriving Show

-- | Associativity of an operator.
data Assoc a
  = AssocNone { assocInfo :: a } -- ^ non-associative operator (declared with @infix@)
  | AssocLeft { assocInfo :: a } -- ^ left-associative operator (declared with @infixl@)
  | AssocRight { assocInfo :: a } -- ^ right-associative operator (declared with @infixr@)
  deriving Show
  
-- | Numeric precedence of an operator
data Precedence a
  = Precedence { precedenceValue :: Int
               , precedenceInfo :: a
               } deriving Show
               
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
  deriving Show
               
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
  deriving Show
               