module Language.Haskell.Refactor.AST.Module where

import SourceCode.ASTElems
import Language.Haskell.Refactor.AST.Base
import Language.Haskell.Refactor.AST.Decl

data Module a 
  = Module { modHead    :: ASTMaybe ModuleHead a
           , modPragmas :: ASTList ModulePragma a
           , modImports :: ASTList ImportDecl a
           , modDecl    :: ASTList Decl a
           , modInfo    :: a 
           } deriving Show

data ModuleHead a
  = ModuleHead { mhName    :: Name a
               , mhExports :: ASTMaybe ExportSpecList a
               , mhInfo    :: a
               } deriving Show

data ExportSpecList a
  = ExportSpecList { espExports :: ASTList ExportSpec a 
                   , espInfo    :: a
                   } deriving Show
  
data ExportSpec a
  = ExportDecl { exportName    :: Name a
               , exportSubspec :: ASTMaybe ExportSubSpec a
               , exportInfo    :: a
               } -- Export a declaration
  | ExportModule { exportName    :: Name a
                 , exportInfo    :: a
                 }
  deriving Show

data ExportSubSpec a
  = ExportSubSpecAll  { essInfo :: a } -- @T(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | ExportSubSpecList { essList :: ASTList Name a
                      , essInfo :: a 
                      } -- @T(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
  deriving Show
                 
data ModulePragma a
  = LanguagePragma { lpPragmas :: ASTList Name a
                   , lpInfo    :: a 
                   }  -- ^ LANGUAGE pragma
  | OptionsPragma  { opTool    :: ASTMaybe Name a
                   , opStr     :: String
                   , opInfo    :: a 
                   } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
  | AnnModulePragma { ampExpr  :: Expr a
                    , ampInfo  :: a
                    } -- ^ ANN pragma with module scope
  deriving Show
                      
data ImportDecl a
  = ImportDecl { importModule       :: Name a
               , importQualified    :: ASTMaybe ImportQualified a
               , importSource       :: ASTMaybe ImportSource a
               , importSafe         :: ASTMaybe ImportSafe a
               , importPkg          :: ASTMaybe Name a
               , importAs           :: ASTMaybe ImportRenaming a
               , importInfo         :: a
               } -- ^ An import declaration
  deriving Show
               
data ImportQualified a  = ImportQualified   { importQualifiedInfo :: a } deriving Show
data ImportSource a     = ImportSource      { importSourceInfo :: a } deriving Show
data ImportSafe a       = ImportSafe        { importSafeInfo :: a } deriving Show
data TypeNamespace a    = TypeNamespace     { typeNamespaceInfo :: a } deriving Show

data ImportRenaming a = ImportRenaming { importRenamingName :: Name a
                                       , importRenamingInfo :: a 
                                       } deriving Show
               