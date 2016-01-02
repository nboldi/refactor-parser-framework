module Language.Haskell.Refactor.Parser.Decl where

import Text.Parsec

import qualified Language.Haskell.Refactor.AST.Decl as AST
import qualified Language.Haskell.Refactor.AST.Base as AST
import Language.Haskell.Refactor.Parser.Base

decl :: HaskellParser (AST.Decl BI)
decl = withInfo 
    $ try (AST.ClosedTypeFamilyDecl <$> (symbol "type" *> symbol "family" *> declHead) <*> astOptionMaybe kindConstraint <*> (symbol "where" *> astMany typeEqn))
  <|> AST.TypeFamilyDecl <$> (try (symbol "type" *> symbol "family") *> declHead) <*> astOptionMaybe kindConstraint
  <|> AST.TypeInstDecl <$> (try (symbol "type" *> symbol "instance") *> type_) <*> (symbol "=" *> type_)
  <|> AST.TypeDecl <$> (symbol "type" *> declHead) <*> (symbol "=" *> type_)
  <|> try (AST.DataDecl <$> dataOrNew <*> astOptionMaybe context <*> declHead <*> astMany conDecl <*> astOptionMaybe deriving_)
  <|> AST.GDataDecl <$> dataOrNew <*> astOptionMaybe context <*> declHead <*> astOptionMaybe kindConstraint <*> gadtDeclList <*> astOptionMaybe deriving_
  <|> AST.DataFamilyDecl <$> (try (symbol "data" *> symbol "family") *> astOptionMaybe context) <*> declHead <*> astOptionMaybe kindConstraint
  <|> try (AST.DataInstDecl <$> dataOrNew <*> (symbol "data" *> symbol "instance" *> type_) <*> (symbol "=" *> astMany conDecl))
  <|> AST.GDataInstDecl <$> (symbol "data" *> symbol "instance" *> type_) <*> (symbol "=" *> type_) <*> gadtDeclList
  <|> AST.ClassDecl <$> (symbol "class" *> context) <*> declHead <*> astOptionMaybe funDeps <*> astOptionMaybe classBody
  <|> AST.InstDecl <$> (symbol "instance" *> astOptionMaybe overlapPragma) <*> instanceRule <*> astOptionMaybe instBody
  <|> AST.DerivDecl <$> (symbol "deriving" *> symbol "instance" *> astOptionMaybe overlapPragma) <*> instanceRule
  <|> AST.FixityDecl <$> assoc <*> integer <*> astSepBy1 name comma
  <|> AST.DefaultDecl <$> parens (astSepBy1 type_ comma)
  <|> AST.TypeSignature <$> (name <* symbol "::") <*> type_
  <|> AST.FunBinding <$> funBind
  <|> AST.ForeignImport <$> (try (symbol "foreign" *> symbol "import") *> callConv) 
                        <*> astOptionMaybe safety <*> symbol "::" <*> astOptionMaybe context <*> type_
  <|> AST.ForeignExport <$> (symbol "foreign" *> symbol "export" *> callConv) 
                        <*> name <*> symbol "::" <*> astOptionMaybe context <*> type_
  <|> AST.SpliceDecl <$> (withInfo $ AST.Var <$> name) <|> (symbol "$(" *> expr <* symbol ")")
      
declHead :: HaskellParser (AST.DeclHead BI)
declHead = fail "not implemented yet"
      
funDeps :: HaskellParser (AST.FunDeps BI)
funDeps = fail "not implemented yet"
      
instanceRule :: HaskellParser (AST.InstanceRule BI)
instanceRule = fail "not implemented yet"
      
classBody :: HaskellParser (AST.ClassBody BI)
classBody = fail "not implemented yet"

instBody :: HaskellParser (AST.InstBody BI)
instBody = fail "not implemented yet"

funBind :: HaskellParser (AST.FunBind BI)
funBind = fail "not implemented yet"
      
typeEqn :: HaskellParser (AST.TypeEqn BI)
typeEqn = fail "not implemented yet"    

conDecl :: HaskellParser (AST.ConDecl BI)
conDecl = fail "not implemented yet"
      
gadtDeclList :: HaskellParser (AST.GadtDeclList BI)
gadtDeclList = fail "not implemented yet"
      
gadtDecl :: HaskellParser (AST.GadtDecl BI)
gadtDecl = fail "not implemented yet"
      
deriving_ :: HaskellParser (AST.Deriving BI)
deriving_ = fail "not implemented yet"

expr :: HaskellParser (AST.Expr BI)
expr = fail "not implemented yet"  
      
context :: HaskellParser (AST.Context BI)
context = fail "not implemented yet"    

kindConstraint :: HaskellParser (AST.KindConstraint BI)
kindConstraint = withInfo $ AST.KindConstraint <$> symbol "::" *> kind
   
type_ :: HaskellParser (AST.Type BI)
type_ = fail "not implemented yet"    

kind :: HaskellParser (AST.Kind BI)
kind = fail "not implemented yet"    