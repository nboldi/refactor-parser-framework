module Language.Haskell.Refactor.AST.Module where

import SourceCode.ASTElems
import Language.Haskell.Refactor.AST.Base
import Language.Haskell.Refactor.AST.Decl

data Module a 
  = Module { modHead :: ASTMaybe ModuleHead a
           , modPragmas :: ASTList ModulePragma a
           , modImports :: ASTList ImportDecl a
           , modDecl :: ASTList Decl a
           , modInfo :: a 
           }

data ModuleHead a
  = ModuleHead { mhName :: Name a
               , mhExports :: ASTMaybe ExportSpecList a
               , mhInfo :: a
               }

data ExportSpecList a
  = ExportSpecList { espExports :: ASTList ExportSpec a 
                   , espInfo :: a
                   }
  
data ExportSpec a
  = ExportVar { evNamespace :: ASTMaybe TypeNamespace a
              , evName :: QName a
              , evInfo :: a
              }
  | ExportAbstract { eAbsName :: QName a
                   , eAbsInfo :: a
                   }
  | ExportAll { eAllName :: QName a
              , eAllInfo :: a
              }
  | ExportWith { eWithName :: QName a
               , eWithDecls :: ASTList Name a
               , eWithInfo :: a
               }
  | ExportModule { eModName :: Name a
                 , eModInfo :: a 
                 }
                 
data ModulePragma a
  = LanguagePragma { lpPragmas :: ASTList Name a
                   , lpInfo :: a 
                   }  -- ^ LANGUAGE pragma
  | OptionsPragma  { opTool :: ASTMaybe Name a
                   , opStr :: String
                   , opInfo :: a 
                   } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
  | AnnModulePragma { ampExpr :: Expr a
                    , ampInfo :: a
                    } -- ^ ANN pragma with module scope
                      
data ImportDecl a
  = ImportDecl { importModule       :: Name a
               , importQualified    :: ASTMaybe ImportQualified a
               , importSource       :: ASTMaybe ImportSource a
               , importSafe         :: ASTMaybe ImportSafe a
               , importPkg          :: ASTMaybe Name a
               , importAs           :: ASTMaybe ImportRenaming a
               , importInfo         :: a
               } -- ^ An import declaration
               
data ImportQualified a  = ImportQualified   { importQualifiedInfo :: a }
data ImportSource a     = ImportSource      { importSourceInfo :: a }
data ImportSafe a       = ImportSafe        { importSafeInfo :: a }
data TypeNamespace a    = TypeNamespace     { typeNamespaceInfo :: a }

data ImportRenaming a = ImportRenaming { importRenamingName :: Name a
                                       , importRenamingInfo :: a 
                                       }
               