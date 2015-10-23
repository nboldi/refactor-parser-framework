{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, NamedFieldPuns, DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ImpredicativeTypes, UndecidableInstances #-}

-- | Extends 'MiniC.AST' with node information types, and class instances for AST nodes.
module MiniC.Representation where

import GHC.Generics (Generic)
import SourceCode.ASTElems
import MiniC.AST
import SourceCode.SourceNotation
import SourceCode.SourceTree
import SourceCode.ToSourceTree
import SourceCode.ASTNode
import SourceCode.SourceInfo
import Data.Typeable
import Data.Data
import Data.List
import Data.Maybe
import Data.Function
import Text.Parsec hiding ((<|>), optional, many)
import Text.Parsec.PosOps
import Data.Data.Lens
import Control.Lens
import Control.Applicative
import Debug.Trace

-- * Preprocessor-handled source elements
  
data Comment = LineComment String
             | BlockComment String
             | DocComment String
             
data AttributeList a 
  = AttributeList { attributes    :: [Attribute a]
                  , attributeInfo :: a
                  } deriving (Show, Eq)
    
data Attribute a
  = Alias        { attribAlias          :: String
                 , attribInfo           :: a 
                 }                      
  | Aligned      { attribAlign          :: Int
                 , attribInfo           :: a
                 }                      
  | AllocSize    { attribAllocSizeArgs  :: [Int]
                 , attribInfo           :: a
                 }                      
  | AlwaysInline { attribInfo           :: a }
  | CDecl        { attribInfo           :: a }
  | Const        { attribInfo           :: a }
  | Constructor  { attribInfo           :: a }
  | Destructor   { attribInfo           :: a }
  | Deprecated   { attribInfo           :: a }
  | Format       { attribFormatFunct    :: String
                 , attribFormatStrArg   :: Int 
                 , attribFormatCheckArg :: Int 
                 , attribInfo           :: a 
                 }
  | Malloc       { attribInfo           :: a }
  | Mode         { attribMode           :: ModeSpec 
                 , attribInfo           :: a
                 }
  | NoInline     { attribInfo           :: a }
  | NoReturn     { attribInfo           :: a }
  | Optimize     { attribOptArgs        :: [Either String Int]
                 , attribInfo           :: a 
                 }
  | Packed       { attribInfo           :: a }
  | Pure         { attribInfo           :: a }
  | RegParam     { attribRegNum         :: Int
                 , attribInfo           :: a 
                 }
  | Section      { attribSection        :: String
                 , attribInfo           :: a 
                 }
  | StdCall      { attribInfo           :: a }
  | TransparentUnion { attribInfo           :: a }
  | Unused       { attribInfo           :: a }
  | Used         { attribInfo           :: a }
  | Visibility   { attribVisibility     :: VisibilityMode
                 , attribInfo           :: a 
                 }
  | Weak         { attribInfo           :: a }
  deriving (Show, Eq)
                 
data ModeSpec = ByteMode | WordMode | PointerMode
     deriving (Show, Eq)
data VisibilityMode = DefaultVisibility | HiddenVisibility | InternalVisibility | ProtectedVisibility
     deriving (Show, Eq)
  
  


