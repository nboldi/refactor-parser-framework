module Language.Haskell.Refactor.Parser.Module where

import Text.Parsec

import Language.Haskell.Refactor.AST.Module
import Language.Haskell.Refactor.Parser.Decl
import Language.Haskell.Refactor.Parser.Base

module_ :: HaskellParser (Module BI)
module_ = withInfo $ Module <$> astOptionMaybe moduleHead <*> astMany modulePragma <*> astMany moduleImport <*> astMany decl

moduleHead :: HaskellParser (ModuleHead BI)
moduleHead = withInfo $ ModuleHead <$> (symbol "module" *> name) <*> (astOptionMaybe exportSpecList <* symbol "where")

exportSpecList :: HaskellParser (ExportSpecList BI)
exportSpecList = withInfo $ ExportSpecList <$> parens (astSepBy exportDecl comma)

exportDecl :: HaskellParser (ExportSpec BI)
exportDecl = withInfo $ 
  ExportModule <$> (symbol "module" *> name)
    <|> ExportDecl <$> (optional (symbol "pattern") *> name) <*> astOptionMaybe exportSubSpecification
    
exportSubSpecification :: HaskellParser (ExportSubSpec BI)
exportSubSpecification = withInfo $ parens (symbol ".." *> return ExportSubSpecAll 
                                              <|> ExportSubSpecList <$> astMany1 name)

modulePragma :: HaskellParser (ModulePragma BI)
modulePragma = undefined

moduleImport :: HaskellParser (ImportDecl BI)
moduleImport = undefined
