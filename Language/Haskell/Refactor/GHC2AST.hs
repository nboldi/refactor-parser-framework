module Language.Haskell.Refactor.GHC2AST where

import Language.Haskell.Refactor.AST as AST
import SourceCode.ASTElems as AST
import SourceCode.InfoTypes as AST
import SourceCode.SourceInfo as AST
import Text.Parsec.PosOps as AST
import Text.Parsec.Pos as AST

import Data.Maybe

import HsSyn as GHC
import Module as GHC
import SrcLoc as GHC
import RdrName as GHC
import Name as GHC hiding (Name)
import BasicTypes as GHC
import Outputable as GHC
import FastString as GHC

import Data.List.Split

trfModule :: Located (HsModule RdrName) -> AST.Module RI
trfModule (L loc (HsModule name exports imports decls deprec haddock)) 
  = AST.Module (trfModuleHead name exports)
               (trfPragmas deprec haddock)
               (trfImports imports)
               (trfDecls decls)
               (trfLoc loc)
       
trfModuleHead :: Maybe (Located ModuleName) -> Maybe (Located [LIE RdrName]) -> ASTMaybe ModuleHead RI 
trfModuleHead (Just mn) exports 
  = ASTJust (ModuleHead (trfModuleNameL mn) (trfExportList exports) noNodeInfo)
trfModuleHead Nothing _ = ASTNothing

trfPragmas :: Maybe (Located WarningTxt) -> Maybe LHsDocString -> ASTList ModulePragma RI
trfPragmas = undefined

trfExportList :: Maybe (Located [LIE RdrName]) -> ASTMaybe ExportSpecList RI
trfExportList Nothing           = ASTNothing
trfExportList (Just (L l exps)) = ASTJust (ExportSpecList (wrapASTList (mapMaybe trfExport exps) (trfLoc l)) 
                                                                       (trfLoc l))
  
trfExport :: LIE RdrName -> Maybe (ExportSpec RI)
trfExport (L l (IEVar n)) = Just (ExportDecl (trfName n) ASTNothing (trfLoc l))
trfExport (L l (IEThingAbs n)) = Just (ExportDecl (trfName n) ASTNothing (trfLoc l))
-- TODO: used annotations to specify subspecifier locations
trfExport (L l (IEThingAll n)) 
  = Just (ExportDecl (trfName n) (ASTJust (ExportSubSpecAll noNodeInfo)) (trfLoc l))
trfExport (L l (IEThingWith n ls)) 
  = Just (ExportDecl (trfName n) 
                     (ASTJust (ExportSubSpecList (wrapASTList (map trfName ls) noNodeInfo) noNodeInfo)) 
         (trfLoc l))
trfExport (L l (IEModuleContents n)) = Just (ExportModule (trfModuleNameL n) (trfLoc l))
trfExport _ = Nothing -- documentation. TODO: unify ranges
  
trfImports :: [LImportDecl RdrName] -> ASTList AST.ImportDecl RI
trfImports imps = wrapASTList (map trfImport imps) noNodeInfo

trfImport :: LImportDecl RdrName -> AST.ImportDecl RI
trfImport = undefined

trfDecls :: [LHsDecl RdrName] -> ASTList Decl RI
trfDecls = undefined
  
trfName :: Located RdrName -> Name RI
trfName (L loc (Unqual n)) = Name astNil (trfSimplName n) (trfLoc loc)
trfName (L loc (Qual mn n)) = Name (trfModuleName mn) (trfSimplName n) (trfLoc loc)
trfName (L loc (Orig m n)) = Name (trfModuleName (moduleName m)) (trfSimplName n) (trfLoc loc)
trfName (L loc (Exact n)) 
  = Name (maybe astNil (trfModuleName . moduleName) (nameModule_maybe n)) 
         (trfSimplName (nameOccName n)) (trfLoc (nameSrcSpan n))

trfSimplName :: OccName -> SimpleName RI
trfSimplName n = SimpleName (pprStr n) noNodeInfo

trfModuleName :: ModuleName -> ASTList SimpleName RI
trfModuleName mn = wrapASTList (map (\n -> SimpleName n noNodeInfo) 
                                    (splitOn "." (moduleNameString mn))) noNodeInfo

trfModuleNameL :: Located ModuleName -> Name RI
trfModuleNameL (L loc mn) = case viewAstListReverse (trfModuleName mn) of 
  Just (init, last) -> Name init last (trfLoc loc)
  Nothing ->           error "Located ModuleName empty"
     
trfLoc :: SrcSpan -> RI
trfLoc loc = NodeInfo (case loc of RealSrcSpan rss -> RangeInfo (trfRealLoc rss)
                                   UnhelpfulSpan _ -> noNodeInfo) ()
                                   
trfRealLoc :: RealSrcSpan -> SourceRange
trfRealLoc rss 
  = SourceRange (newPos (unpackFS $ srcSpanFile rss) (srcSpanStartLine rss) (srcSpanStartCol rss))
                (newPos (unpackFS $ srcSpanFile rss) (srcSpanEndLine rss)   (srcSpanEndCol rss))

                
                
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                
