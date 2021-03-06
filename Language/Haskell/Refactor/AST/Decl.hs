module Language.Haskell.Refactor.AST.Decl where

import Language.Haskell.Refactor.AST.Base

import SourceCode.ASTElems

data Decl a
  = TypeDecl { declHead :: DeclHead a
             , declType :: Type a
             , declInfo :: a
             } -- ^ A type synonym ( @type String = [Char]@ )
  | TypeFamilyDecl { declHead :: DeclHead a
                   , declKind :: ASTMaybe KindConstraint a
                   , declInfo :: a
                   } -- ^ A type family declaration
  | ClosedTypeFamilyDecl { declHead :: DeclHead a
                         , declKind :: ASTMaybe KindConstraint a
                         , declDecl :: ASTList TypeEqn a
                         , declInfo :: a
                         } -- ^ A closed type family declaration
  | DataDecl { declNewtype :: DataOrNewtypeKeyword a
             , declCtx  :: ASTMaybe Context a
             , declHead :: DeclHead a
             , declCons :: ASTList ConDecl a
             , declDeriving :: ASTMaybe Deriving a
             , declInfo :: a
             } -- ^ A data or newtype declaration.
  | GDataDecl { declNewtype :: DataOrNewtypeKeyword a
              , declCtx  :: ASTMaybe Context a
              , declHead :: DeclHead a
              , declKind :: ASTMaybe KindConstraint a
              , declGadt :: GadtDeclList a
              , declDeriving :: ASTMaybe Deriving a
              , declInfo :: a
              } -- ^ A data or newtype declaration.
  | DataFamilyDecl { declCtx  :: ASTMaybe Context a
                   , declHead :: DeclHead a
                   , declKind :: ASTMaybe KindConstraint a
                   , declInfo :: a
                   } -- ^ Data family declaration
  | TypeInstDecl { declInstance :: Type a
                 , declAssignedType :: Type a
                 , declInfo :: a
                 } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl { declNewtype :: DataOrNewtypeKeyword a
                 , declInstance :: Type a
                 , declCons :: ASTList ConDecl a
                 , declInfo :: a
                 } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl { declNewtype :: DataOrNewtypeKeyword a
                  , declInstance :: Type a
                  , declKind :: ASTMaybe KindConstraint a
                  , declGadt :: GadtDeclList a
                  , declInfo :: a
                  } -- ^ Data instance declaration (@ data instance T = Con1 | Con2 @)
  | ClassDecl { declCtx :: ASTMaybe Context a
              , declHead :: DeclHead a
              , declFunDeps :: ASTMaybe FunDeps a
              , declBody :: ASTMaybe ClassBody a
              , declInfo :: a
              } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl { declOverlap :: ASTMaybe OverlapPragma a
             , declInstRule :: InstanceRule a
             , declInstDecl :: ASTMaybe InstBody a
             , declInfo :: a
             } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | DerivDecl { declOverlap :: ASTMaybe OverlapPragma a
              , declInstRule :: InstanceRule a
              , declInfo :: a
              } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl { declAssoc :: Assoc a
               , declPrecedence :: Precedence a
               , declOperators :: ASTList Name a
               , declInfo :: a
               }
  | DefaultDecl { declTypes :: ASTList Type a
                , declInfo :: a
                } -- ^ Default types (@ default (T1, T2) @)
  | TypeSignature { declName :: Name a
                  , declType :: Type a
                  , declInfo :: a
                  } -- ^ type signature (@ f :: Int -> Int @)
  | FunBinding { declFunBind :: FunBind a
               , declInfo :: a
               } -- ^ function binding (@ f x = 12 @)
  | ForeignImport { declCallConv :: CallConv a
                  , declSafety :: ASTMaybe Safety a
                  , declName :: Name a
                  , declType :: Type a
                  , declInfo :: a
                  } -- ^ foreign import (@ foreign import foo :: Int -> IO Int @)
  | ForeignExport { declCallConv :: CallConv a
                  , declName :: Name a
                  , declType :: Type a
                  , declInfo :: a
                  } -- ^ foreign export (@ foreign export ccall foo :: Int -> IO Int @)
  -- | Pragma { tlPragma :: TopLevelPragma a } -- ^ top level pragmas
  | SpliceDecl { declExpr :: Expr a
               , declInfo :: a
               } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
  deriving Show
       
-- | The list of declarations that can appear in a typeclass
data ClassBody a
  = ClassBody { cbElements :: ASTList ClassElement a
              , cbInfo :: a
              } deriving Show
              
-- | A list of GADT declarations with the @where@ keyword
data GadtDeclList a 
  = GadtDeclList { gadtList :: ASTList GadtDecl a
                 , gadtInfo :: a
                 } deriving Show
                 
                 
data ClassElement a
  = ClsDecl { cleDecl :: Decl a
            , cleInfo :: a
            } -- ^ ordinary declaration: @ f :: A -> B @
  | ClsDataFam { cldfCtx :: ASTMaybe Context a
               , cldfHead :: DeclHead a
               , cldfKind :: ASTMaybe Kind a
               , cleInfo :: a
               } -- ^ declaration of an associated data type: @ data T x :: * @ 
  | ClsTypeFam { cltfHead :: DeclHead a
               , cltfKind :: ASTMaybe Kind a
               , cleInfo :: a
               } -- ^ declaration of an associated type synonym: @ type T x :: * @ 
  | ClsTypeDef { cltdHead :: DeclHead a
               , cltdKind :: ASTMaybe Kind a
               , cleInfo :: a
               } -- ^ default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | ClsDefSig { cldName :: Name a
              , cldType :: Type a
              , cleInfo :: a
              } -- ^ default signature (by using DefaultSignatures): @ default enum :: (Generic a, GEnum (Rep a)) => [a] @
  deriving Show
       
data DeclHead a
  = DeclHead { dhName :: Name a 
             , dhInfo :: a
             } -- ^ type or class name
  | DHParen  { dhBody :: DeclHead a
             , dhInfo :: a
             }
  | DHApp    { dhAppFun :: DeclHead a
             , dhAppOperand :: TyVar a 
             , dhInfo :: a
             }
  | DHInfix  { dhInfixName :: Name a 
             , dhInfixLeft :: TyVar a
             , dhInfo :: a
             } -- ^ infix application of the type/class name to the left operand
 deriving Show
       
data InstBody a
  = InstBody { instBodyDecls :: ASTList InstBodyDecl a
             , instBodyInfo :: a
             } deriving Show

-- | Declarations inside an instance declaration.
data InstBodyDecl a
  = InstBodyNormalDecl   { instBodyDeclFunbind :: FunBind a
                         , instBodyDeclInfo :: a
                         } -- ^ a normal declaration (@ f x = 12 @)
  | InstBodyTypeDecl     { instBodyLhsType :: Type a
                         , instBodyRhsType :: Type a
                         , instBodyDeclInfo :: a
                         } -- ^ an associated type definition (@ type A X = B @)
  | InstBodyDataDecl     { instBodyDataNew :: DataOrNewtypeKeyword a
                         , instBodyLhsType :: Type a
                         , instBodyDataCons :: ASTList ConDecl a
                         , instBodyDerivings :: ASTMaybe Deriving a
                         , instBodyDeclInfo :: a
                         } -- ^ an associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { instBodyDataNew :: DataOrNewtypeKeyword a
                         , instBodyLhsType :: Type a
                         , instBodyDataKind :: ASTMaybe Kind a
                         , instBodyGadtCons :: ASTList GadtDecl a
                         , instBodyDerivings :: ASTMaybe Deriving a
                         , instBodyDeclInfo :: a
                         } -- ^ an associated data type implemented using GADT style
  deriving Show
  

data GadtDecl a
  = GadtDecl { gdName :: Name a
             , gdFields :: ASTMaybe (ASTList FieldDecl) a
             , gdType :: Type a
             , gdInfo :: a
             } deriving Show
         
-- | A list of functional dependencies: @ ... | a -> b, c -> d @         
data FunDeps a
  = FunDeps { funDeps :: ASTList FunDep a
            , funDepsInfo :: a
            } deriving Show
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep a
  = FunDep { funDepLhs :: ASTList Name a
           , funDepRhs :: ASTList Name a
           , funDepInfo :: a
           } deriving Show
  
data ConDecl a
  = ConDecl { conDeclName :: Name a
            , conDeclArgs :: ASTList Type a
            , conDeclInfo :: a
            } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl { conDeclName :: Name a
               , conDeclFields :: ASTList FieldDecl a
               , conDeclInfo :: a
               } -- ^ record data constructor (@ C { n1 :: t1, n2 :: t2 } @)
  | InfixConDecl { icdName :: Name a
                 , icdLhs :: Type a
                 , icdRhs :: Type a
                 , icdInfo :: a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  deriving Show
  
data FieldDecl a
  = FieldDecl { fieldNames :: ASTList Name a
              , fieldType :: Type a
              , fieldInfo :: a
              } deriving Show
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving a
  = DerivingOne { oneDerived :: InstanceRule a
                , derivingInfo :: a
                }
  | Derivings   { allDerived :: ASTList InstanceRule a
                , derivingInfo :: a
                }
  deriving Show
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule a
  = InstanceRule { irVars :: ASTMaybe (ASTList TyVar) a
                 , irCtx :: ASTMaybe Context a
                 , irHead :: InstanceHead a
                 , irInfo :: a
                 }
  | InstanceParen { irRule :: InstanceRule a
                  , irInfo :: a
                  }
  deriving Show

data InstanceHead a
  = InstanceHeadCon { ihConName :: Name a
                    , ihInfo :: a
                    } -- ^ type or class name
  | InstanceHeadInfix { ihLeftOp :: Type a
                      , ihOperator :: Name a
                      , ihInfo :: a
                      } -- ^ infix application of the type/class name to the left operand
  | InstanceHeadParen { ihHead :: InstanceHead a
                      , ihInfo :: a
                      } -- ^ parenthesized instance head
  | InstanceHeadApp { ihFun :: InstanceHead a
                    , ihType :: Type a
                    , ihInfo :: a
                    } -- ^ application to one more type
  deriving Show
        
data TypeEqn a
  = TypeEqn { teLhs :: Type a
            , teRhs :: Type a
            , teInfo :: a
            } -- ^ type equations as found in closed type families (@ T A = S @)
  deriving Show
  
data KindConstraint a 
  = KindConstraint { kindConstr :: Kind a
                   , kindConstrInfo :: a
                   } -- ^ kind constraint (for example on declarations, with @::@)
  deriving Show
   
-- data TopLevelPragma a
  -- = RulePragma    { pragmaRule :: ASTList Rule a
                  -- , pragmaInfo :: a
                  -- }
  -- | DeprPragma    { pragmaObjects :: ASTList Name a
                  -- , pragmaMessage :: String
                  -- , pragmaInfo :: a
                  -- }
  -- | WarningPragma { pragmaObjects :: ASTList Name a
                  -- , pragmaMessage :: String
                  -- , pragmaInfo :: a
                  -- }
  -- | AnnPragma     { pragmaAnnotation :: Annotation a
                  -- , pragmaInfo :: a
                  -- }
  -- | MinimalPragma { pragmaFormula :: Maybe MinimalFormula a
                  -- , pragmaInfo :: a
                  -- }
                  
-- data Annotation a
  -- = NameAnnotation   { annotateType :: ASTMaybe TypeKeyword
                     -- , annotateName :: Name a
                     -- , annotateExpr :: Expr a
                     -- , annotatoinInfo :: a
                     -- }
  -- | ModuleAnnotation { annotateExpr :: Expr a
                     -- , annotatoinInfo :: a
                     -- }
                     
-- Rule pragmas omitted
   
----------------------------------------------------
-- Types -------------------------------------------
----------------------------------------------------
   
data TyVar a 
  = TyVarDecl { tyVarName :: Name a
              , tyVarKind :: ASTMaybe Kind a
              , tyVarInfo :: a
              } deriving Show
           
data Type a
  = TyForall   { tfaBounded :: ASTMaybe TyVar a
               , tfaCtx :: ASTMaybe Context a
               , tfaType :: Type a
               , typeInfo :: a
               } -- ^ forall types (@ forall x y . type @)
  | TyFun      { tfParam :: Type a
               , tfResult :: Type a
               , tfaType :: Type a
               , typeInfo :: a
               } -- ^ function types (@ a -> b @)
  | TyTuple    { ttElements :: ASTList Type a
               , typeInfo :: a
               } -- ^ tuple types (@ (a,b) @)
  | TyUnbTuple { ttElements :: ASTList Type a
               , typeInfo :: a
               } -- ^ unboxed tuple types (@ (#a,b#) @)
  | TyList     { tlElement :: Type a
               , typeInfo :: a 
               } -- ^ list type with special syntax (@ [a] @)
  | TyParArray { tpaElement :: Type a
               , typeInfo :: a 
               } -- ^ parallel array type (@ [:a:] @)
  | TyApp      { taCon :: Type a
               , taArg :: Type a
               , typeInfo :: a
               } -- ^ type constructor application (@ F a @)
  | TyVar      { tvName :: Name a
               , typeInfo :: a
               } -- ^ type variable (@ a @)
  | TyCon      { tcName :: Name a
               , typeInfo :: a
               } -- ^ type constructor (@ T @)
  | TyParen    { tpType :: Type a
               , typeInfo :: a 
               } -- ^ type surrounded by parentheses (@ (T a) @)
  | TyInfix    { tiLeft :: Type a 
               , tiOperator :: Name a
               , tiRight :: Type a
               , typeInfo :: a
               } -- ^ infix type constructor (@ (a <: b) @)
  | TyKinded   { tkType :: Type a
               , tkKind :: Kind a
               , typeInfo :: a
               } -- ^ type with explicit kind signature (@ a :: * @)
  | TyPromoted { tpPromoted :: Promoted a
               , typeInfo :: a
               } -- a promoted data type with -XDataKinds (@ '3 @).
  | TySplice   { tsSplice :: Splice a
               , typeInfo :: a
               } -- ^ a Template Haskell splice type (@ $(genType) @).
  | TyBang     { typeInner :: Type a
               , typeInfo :: a
               } -- ^ Strict type marked with "!".
  | TyUnpack   { typeInner :: Type a
               , typeInfo :: a
               } -- ^ Type marked with UNPACK pragma.
  deriving Show

data Kind a
  = KindStar { kindInfo :: a } -- ^ *, the kind of types
  | KindBang { kindInfo :: a } -- ^ !, the kind of unboxed types
  | KindFn { kindLeft :: Kind a
           , kindRight :: Kind a
           , kindInfo :: a
           } -- ^ ->, the kind of type constructor
  | KindParen { kindParen :: Kind a
              , kindInfo :: a 
              } -- ^ a parenthesised kind
  | KindVar { kindVar :: Name a
            , kindInfo :: a 
            } -- ^ kind variable (using PolyKinds extension)
  | KindApp { kindAppFun :: Kind a
            , kindAppArg :: Kind a 
            , kindInfo :: a 
            } -- ^ kind application (@ k1 k2 @)
  | KindTuple { kindTuple :: ASTList Kind a
              , kindInfo :: a 
              } -- ^ a promoted tuple (@ '(k1,k2,k3) @)
  | KindList { kindList :: ASTList Kind a
             , kindInfo :: a 
             } -- ^ a promoted list literal (@ '[k1,k2,k3] @)
  deriving Show
  
data Context a
  = ContextOne { contextAssertion :: Assertion a
               , contextInfo :: a
               } -- ^ one assertion (@ C a => ... @)
  | ContextMulti { contextAssertions :: ASTList Assertion a
                 , contextInfo :: a
                 } -- ^ a set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)
  deriving Show

data Assertion a
  = ClassAssert     { assertClsName :: Name a
                    , assertTypes :: ASTList Type a
                    , assertInfo :: a
                    }
  | AppAssert       { assertConstrName :: Name a
                    , assertTypes :: ASTList Type a
                    , assertInfo :: a
                    }
  | InfixAssert     { assertLhs :: Type a
                    , assertOp :: Name a
                    , assertRhs :: Type a
                    , assertInfo :: a
                    }
  | EqualAssert     { assertLhs :: Type a
                    , assertRhs :: Type a
                    , assertInfo :: a
                    }
  | ParenAssert     { assertInner :: Assertion a
                    , assertInfo :: a
                    }
  | WildcardAssert  { assertWildcardName :: ASTMaybe Name a
                    , assertInfo :: a
                    }
  deriving Show
                 
-- | Haskell expressions
data Expr a
  = Var               { exprName :: Name a
                      , exprInfo :: a
                      } -- ^ a variable (@ a @)
  | Con               { exprName :: Name a
                      , exprInfo :: a
                      } -- ^ data constructor (@Point@ in @Point 1 2@)
  | Lit               { exprLit :: Literal a
                      , exprInfo :: a
                      } -- ^ primitive literal
  | InfixApp          { exprLhs :: Expr a
                      , exprOperator :: Name a
                      , exprRhs :: Expr a
                      , exprInfo :: a
                      } -- ^ Infix operator application (@ a + b @)
  | App               { exprFun :: Expr a
                      , exprArg :: Expr a
                      , exprInfo :: a
                      } -- ^ Function application (@ f 4 @)
  -- unary minus omitted
  | Lambda            { exprBindings :: ASTList Pattern a -- ^ at least one
                      , exprInner :: Expr a
                      , exprInfo :: a
                      } -- ^ lambda expression (@ \a b -> a + b @)
  | Let               { exprFunBind :: ASTList FunBind a
                      , exprInner :: Expr a
                      , exprInfo :: a
                      }
  | If                { exprCond :: Expr a
                      , exprThen :: Expr a
                      , exprElse :: Expr a
                      , exprInfo :: a
                      }
  | MultiIf           { exprIfAlts :: ASTList GuardedRhs a
                      , exprInfo :: a
                      }
  | Case              { exprCase :: Expr a
                      , exprAlts :: ASTList Alt a
                      , exprInfo :: a
                      }
  | Do                { doKind :: DoKind a
                      , exprStmts :: ASTList Stmt a
                      , exprInfo :: a
                      }
  | Tuple             { tupleElems :: ASTList Expr a
                      , exprInfo :: a
                      }
  | UnboxedTuple      { tupleElems :: ASTList Expr a
                      , exprInfo :: a
                      }
  | TupleSection      { tupleSectionElems :: ASTList (ASTMaybe Expr) a
                      , exprInfo :: a
                      }
  | BoxedTupleSection { tupleSectionElems :: ASTList (ASTMaybe Expr) a
                      , exprInfo :: a
                      }
  | List              { listElems :: ASTList Expr a
                      , exprInfo :: a
                      } -- ^ List expression: @[1,2,3]@
  | ParArray          { listElems :: ASTList Expr a
                      , exprInfo :: a
                      } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | Paren             { exprInner :: Expr a
                      , exprInfo :: a
                      }
  | LeftSection       { exprLhs :: Expr a
                      , exprOperator :: Name a
                      , exprInfo :: a
                      } -- ^ Left operator section: @(1+)@
  | RightSection      { exprOperator :: Name a
                      , exprRhs :: Expr a
                      , exprInfo :: a
                      } -- ^ Right operator section: @(+1)@
  | RecCtorExpr       { exprRecName :: Name a
                      , exprRecFields :: ASTList FieldUpdate a
                      , exprInfo :: a
                      } -- ^ Record value construction: @Point { x = 3, y = -2 }@
  | RecUpdateExpr     { exprToUpdate :: Expr a
                      , exprRecFields :: ASTList FieldUpdate a
                      , exprInfo :: a
                      } -- ^ Record value update: @p1 { x = 3, y = -2 }@
  | Enum              { enumFrom :: Expr a
                      , enumThen :: ASTMaybe Expr a
                      , enumTo :: ASTMaybe Expr a
                      , exprInfo :: a
                      }
  | ParArrayEnum      { parEnumFrom :: Expr a
                      , parEnumThen :: ASTMaybe Expr a
                      , parEnumTo :: Expr a
                      , exprInfo :: a
                      }
  | ListComp          { compExpr :: Expr a
                      , compBody :: ASTList CompStmt a
                      } -- ^ List comprehension  
  | ParListComp       { compExpr :: Expr a
                      , parCompBody :: ASTList (ASTList CompStmt) a
                      } -- ^ Parallel list comprehension: @ [ (x, y) | x <- xs | y <- ys ] @
  | ParArrayComp      { compExpr :: Expr a
                      , parCompBody :: ASTList (ASTList CompStmt) a
                      } -- ^ List comprehension  
  | TypeSig           { exprInner :: Expr a
                      , exprSig :: Type a
                      , exprInfo :: a
                      }
  -- Template Haskell
  | VarQuote          { quotedName :: Name a
                      , exprInfo :: a
                      } -- ^ @'x@ for template haskell reifying of expressions
  | TypeQuote         { quotedName :: Name a
                      , exprInfo :: a
                      } -- ^ @''T@ for template haskell reifying of types
  | BracketExpr       { bracket :: Bracket a
                      , exprInfo :: a
                      } -- ^ template haskell bracket expression, for example: @[| x |]@
  | Splice            { innerExpr :: Expr a
                      , exprInfo :: a
                      } -- ^ template haskell splice expression, for example: @$(gen a)@ or @$x@
  | QuasiQuote        { qqExprName :: Name a
                      , qqExprBody :: QQString a
                      , exprInfo :: a
                      } -- ^ template haskell quasi-quotation: @[$quoter|str]@
  -- | ExprPragma        { exprPragma :: ExprPragma a 
                      -- , exprInfo :: a
                      -- }
  -- Arrows
  | Proc              { procPattern :: Pattern a
                      , procExpr :: Expr a
                      , exprInfo :: a
                      }
  | ArrowApp          { exprLhs :: Expr a
                      , arrowAppl :: ArrowAppl a
                      , exprRhs :: Expr a
                      }
  | LamCase           { exprAlts :: ASTList Alt a
                      , exprInfo :: a
                      } -- ^ lambda case: @\case 0 -> 1; 1 -> 2@
  -- XML expressions omitted
  deriving Show
          
data Stmt a
  = BindStmt { stmtPattern :: Pattern a
             , stmtExpr :: Expr a
             , stmtInfo :: a
             }
  | ExprStmt { stmtExpr :: Expr a
             , stmtInfo :: a
             }
  | LetStmt  { stmtBinds :: Binds a
             , stmtInfo :: a
             }
  | RecStmt  { stmtRecBinds :: ASTList Stmt a
             , stmtInfo :: a
             } -- ^ a recursive binding group for arrows
  deriving Show
          
data CompStmt a
  = CompStmt   { compStmt :: Stmt a 
               , compInfo :: a
               }
  | ThenStmt   { thenExpr :: Expr a 
               , byExpr :: ASTMaybe Expr a
               , compInfo :: a
               }
  | GroupStmt  { byExpr :: ASTMaybe Expr a
               , usingExpr :: ASTMaybe Expr a
               , compInfo :: a
               } -- ^ byExpr or usingExpr must have a value
  deriving Show
          
-- | Function binding
data FunBind a
  = FunBind { funBindMatches :: ASTList Match a 
            , funBindInfo :: a
            }        
  deriving Show            
          
data Pattern a
  = VarPat { patternName :: Name a
           , patternInfo :: a
           }
  | LitPat { patternLiteral :: Literal a
           , patternInfo :: a
           }
  | InfixPat { patternLhs :: Pattern a
             , patternOp :: Name a
             , patternRhs :: Pattern a
             , patternInfo :: a
             }
  | AppPat { patternCon :: Name a
           , patternArg :: Pattern a
           , patternInfo :: a
           }
  | TuplePat { patternElems :: ASTList Pattern a
             , patternInfo :: a
             }
  | UnboxTuplePat { patternElems :: ASTList Pattern a
                  , patternInfo :: a
                  }
  | ListPat { patternElems :: ASTList Pattern a
            , patternInfo :: a
            }
  | ParenPat { patternInner :: Pattern a
             , patternInfo :: a
             }
  | RecPat { patternName :: Name a
           , patternFields :: ASTList PatternField a
           , patternInfo :: a
           }
  | AsPat { patternName :: Name a
          , patternInner :: Pattern a
          , patternInfo :: a
          } -- ^ As-pattern: @ls\@(hd:_)@
  | WildPat { patternInfo :: a } -- ^ Wildcard pattern: @_@
  | IrrPat { patternInner :: Pattern a
           , patternInfo :: a
           } -- ^ Irrefutable pattern: @~(x:_)@
  | BangPat { patternInner :: Pattern a
            , patternInfo :: a
            } -- ^ Bang pattern: @!x@
  | TypeSigPat { patternInner :: Pattern a
               , patternType :: Type a
               , patternInfo :: a
               } -- ^ Pattern with type signature: @_ :: Int@
  | ViewPat { patternExpr :: Expr a
            , patternInner :: Pattern a
            , patternInfo :: a
            } -- ^ View pattern: @f -> Just 1@
  -- regular list pattern omitted
  -- xml patterns omitted
  | QuasiQuotePat { qqPatternName :: Name a
                  , qqPatternBody :: QQString a
                  , patternInfo :: a
                  }
  deriving Show
                  
data PatternField a 
  = NormalFieldPattern   { fieldPatternName :: Name a
                         , fieldPattern :: Pattern a
                         , fieldPatternInfo :: a
                         }
  | FieldPunPattern      { fieldPatternName :: Name a
                         , fieldPatternInfo :: a
                         }
  | FieldWildcardPattern { fieldPatternInfo :: a }
  deriving Show
                       
data Splice a
  = IdSplice { spliceId :: Name a
             , spliceInfo :: a 
             }
  | ParenSplice { spliceExpr :: Expr a
                , spliceInfo :: a
                }
  deriving Show
               
data QQString a
  = QQString { qqString :: String
             , qqInfo :: a
             } deriving Show
              
data Binds a
  = DeclBindings { bindingDecls :: ASTList Decl a
                 , bindingInfo :: a
                 } deriving Show
   
-- | Clause of function binding   
data Match a
  = Match { matchName :: Name a
          , matchArgs :: ASTList Pattern a
          , matchType :: ASTMaybe Type a
          , matchRhs :: Rhs a
          , matchBinds :: ASTMaybe Binds a
          , matchInfo :: a
          } deriving Show
    
-- | Clause of case expression          
data Alt a
  = Alt { altPattern :: Pattern a
        , altRhs :: Rhs a
        , altBinds :: ASTMaybe Binds a
        , altInfo :: a
        } deriving Show
        
data Rhs a
  = UnguardedRhs { rhsExpr :: Expr a
                 , rhsInfo :: a
                 }
  | GuardedRhss  { rhsGuards :: ASTList GuardedRhs a
                 , rhsInfo :: a
                 }
  deriving Show
               
data GuardedRhs a
  = GuardedRhs { guardStmts :: ASTList Stmt a
               , guardExpr :: Expr a
               , guardInfo :: a
               } deriving Show
               
data FieldUpdate a 
  = NormalFieldUpdate { fieldName :: Name a
                      , fieldValue :: Expr a
                      , fieldUpdateInfo :: a
                      }
  | FieldPun          { fieldName :: Name a
                      , fieldUpdateInfo :: a
                      }
  | FieldWildcard     { fieldUpdateInfo :: a }
  deriving Show
               
data Bracket a
  = ExprBracket    { bracketExpr :: Expr a
                   , bracketInfo :: a 
                   }
  | PatternBracket { bracketPattern :: Pattern a
                   , bracketInfo :: a 
                   }
  | TypeBracket    { bracketType :: Type a
                   , bracketInfo :: a
                   }
  | DeclBracket    { bracketDecl :: Decl a
                   , bracketInfo :: a
                   }
  deriving Show
                  