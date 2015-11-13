module Language.Haskell.Refactor.AST.Decl where

import Language.Haskell.Refactor.AST.Base

import SourceCode.ASTElems

data Decl a
  = TypeDecl { tdHead :: DeclHead a
             , tdType :: Type a
             , declInfo :: a
             } -- ^ A type synonym ( @type String = [Char]@ )
  | TypeFamilyDecl { tfHead :: DeclHead a
                   , tfKind :: ASTMaybe Kind a
                   , declInfo :: a
                   } -- ^ A type family declaration
  | ClosedTypeFamilyDecl { ctfHead :: DeclHead a
                         , ctfKind :: ASTMaybe Kind a
                         , ctfDecl :: ASTList TypeEqn a
                         , declInfo :: a
                         } -- ^ A closed type family declaration
  | DataDecl { ddType :: DataOrNewKeyword a
             , ddCtx  :: ASTMaybe Context a
             , ddHead :: DeclHead a
             , ddCons :: ASTList ConDecl a
             , ddDeriving :: ASTMaybe Deriving a
             , declInfo :: a
             } -- ^ A data or newtype declaration.
  | GDataDecl { gddType :: DataOrNewKeyword a
              , gddCtx  :: ASTMaybe Context a
              , gddHead :: DeclHead a
              , gddKind :: ASTMaybe Kind a
              , gddCons :: ASTList GadtDecl a
              , gddDeriving :: ASTMaybe Deriving a
              , declInfo :: a
              } -- ^ A data or newtype declaration.
  | DataFamilyDecl { gddCtx  :: ASTMaybe Context a
                   , gddHead :: DeclHead a
                   , gddKind :: ASTMaybe Kind a
                   , declInfo :: a
                   } -- ^ Data family declaration
  | TypeInstDecl { tidInstance :: Type a
                 , tidAssignedType :: Type a
                 , declInfo :: a
                 } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl { didType :: DataOrNewKeyword a
                 , didInstance :: Type a
                 , didCons :: ASTList ConDecl a
                 , declInfo :: a
                 } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl { gdidType :: DataOrNewKeyword a
                  , gdidInstance :: Type a
                  , gdidKind :: Kind a
                  , gdidCons :: ASTList GadtDecl a
                  , declInfo :: a
                  } -- ^ Data instance declaration (@ data instance T = Con1 | Con2 @)
  | ClassDecl { cdCtx :: Context a
              , cdHead :: DeclHead a
              , cdFunDeps :: ASTMaybe FunDeps a
              , cdBody :: ASTMaybe ClassBody a
              , declInfo :: a
              } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl { idOverlap :: ASTMaybe OverlapPragma a
             , idInstRule :: InstanceRule a
             , idInstDecl :: ASTMaybe InstBody a
             , declInfo :: a
             } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | DerivDecl { drdOverlap :: ASTMaybe OverlapPragma a
              , drdInstRule :: InstanceRule a
              , declInfo :: a
              } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl { fdAssoc :: Assoc a
               , fdPrecedence :: Precedence a
               , fdOperators :: ASTList Name a
               , declInfo :: a
               }
  | DefaultDecl { dfdType :: Type a
                , declInfo :: a
                } -- ^ Default types (@ default (T1, T2) @)
  | SpliceDecl { spdExpr :: Expr a
               , declInfo :: a
               } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
  | TypeSig { tsName :: Name a
            , tyType :: Type a
            , declInfo :: a
            } -- ^ type signature (@ f :: Int -> Int @)
  | FunBind { declFunBind :: FunBind a
            , declInfo :: a
            } -- ^ function binding (@ f x = 12 @)
  | ForeignImport { fiCallConv :: CallConv a
                  , fiSafety :: ASTMaybe Safety a
                  , fiName :: Name a
                  , fiType :: Type a
                  , declInfo :: a
                  } -- ^ foreign import (@ foreign import foo :: Int -> IO Int @)
  | ForeignExport { feCallConv :: CallConv a
                  , feName :: Name a
                  , feType :: Type a
                  , declInfo :: a
                  } -- ^ foreign export (@ foreign export ccall foo :: Int -> IO Int @)
  | Pragma { tlPragma :: TopLevelPragma a } -- ^ top level pragmas
                 
data DeclHead a
  = DeclHead { dhName :: Name a 
             , dhInfo :: a
             } -- ^ type or class name
  | DHParen { dhBody :: DeclHead a
            , dhInfo :: a
            }
  | DHApp { dhAppFun :: DeclHead a
          , dhAppOperand :: TyVar a 
          , dhInfo :: a
          }
  | DHInfix { dhInfixName :: Name a 
            , dhInfixLeft :: TyVar a
            , dhInfo :: a
            } -- ^ infix application of the type/class name to the left operand
                 
data Type a
  = TyForall { tfaBounded :: ASTMaybe TyVar a
             , tfaCtx :: ASTMaybe Context a
             , tfaType :: Type a
             , tfaInfo :: a
             } -- ^ forall types (@ forall x y . type @)
  | TyFun { tfParam :: Type a
          , tfResult :: Type a
          , tfaType :: Type a
          } -- ^ function types (@ a -> b @)
  | TyTuple { ttElements :: ASTList Type a
            , ttInfo :: a
            } -- ^ tuple types (@ (a,b) @)
  | TyUnbTuple { ttElements :: ASTList Type a
               , ttInfo :: a
               } -- ^ unboxed tuple types (@ (#a,b#) @)
  | TyList { tlElement :: Type a
           , tlInfo :: a 
           } -- ^ list type with special syntax (@ [a] @)
  | TyParArray { tpaElement :: Type a
               , tpaInfo :: a 
               } -- ^ parallel array type (@ [:a:] @)
  | TyApp { taCon :: Type a
          , taArg :: Type a
          , taInfo :: a
          } -- ^ type constructor application (@ F a @)
  | TyVar { tvName :: Name a } -- ^ type variable (@ a @)
  | TyCon { tcName :: Name a } -- ^ type constructor (@ T @)
  | TyParen { tpType :: Type a, tpInfo :: a } -- ^ type surrounded by parentheses (@ (T a) @)
  | TyInfix { tiLeft :: Type a 
            , tiOperator :: Name a
            , tiRight :: Type a
            , tkInfo :: a
            } -- ^ infix type constructor (@ (a <: b) @)
  | TyKinded { tkType :: Type a
             , tkKind :: Kind a
             , tkInfo :: a
             } -- ^ type with explicit kind signature (@ a :: * @)
  | TyPromoted { tpPromoted :: Promoted a } -- a promoted data type with -XDataKinds (@ '3 @).
  | TySplice { tsSplice :: Splice a
             , tsInfo :: a
             } -- ^ a Template Haskell splice type (@ $(genType) @).
  | TyBang { tbBang :: BangType a
           , tbType :: Type a
           , tbInfo :: a
           } -- ^ Strict type marked with "!" or type marked with UNPACK pragma.

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
  
data TypeEqn a
  = TypeEqn { teLhs :: Type a
            , teRhs :: Type a
            , teInfo :: a
            } -- ^ type equations as found in closed type families (@ T A = S @)
  
data DataOrNewKeyword a
  = DataKeyword { dataKeywordInfo :: a }
  | NewdataKeyword { newdataKeywordInfo :: a }
  
data Context a
  = ContextOne { contextAssertion :: Assertion a
               , contextInfo :: a
               } -- ^ one assertion (@ C a => ... @)
  | ContextMulti { contextAssertions :: ASTList Assertion a
                 , contextInfo :: a
                 } -- ^ a set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)
  
data ConDecl a
  = ConDecl { conDeclName :: Name a
            , conDeclArgs :: ASTList Type a
            , conDeclInfo :: a
            } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl { conDeclName :: Name a
               , conDeclArgs :: ASTList FieldDecl a
               , conDeclInfo :: a
               } -- ^ record data constructor (@ C { n1 :: t1, n2 :: t2 } @)
  | InfixConDecl { icdName :: Name a
                 , icdLhs :: Type a
                 , icdRhs :: Type a
                 , icdInfo :: a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving a
  = DerivingOne { derived :: InstanceRule a
                , derivingInfo :: a
                }
  | Derivings { derived :: ASTList InstanceRule a
              , derivingInfo :: a
              }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule a
  = InstanceRule { irVars :: ASTMaybe (ASTList TypeVar) a
                 , irCtx :: ASTMaybe Context a
                 , irHead :: InstanceHead a
                 , irInfo :: a
                 }
  | InstanceParen { irRule :: InstanceRule a
                  , irInfo :: a
                  }

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
  
-- | A single constructor declaration in a GADT data type declaration.
--
-- If the GADT is declared using the record syntax, e.g.
--
-- >data Ty where
-- >  TCon :: { field1 :: Int, field2 :: Bool } -> Ty
--
-- then the fields are stored as a list of 'FieldDecl's, and the final type
-- (@Ty@ in the above example) is stored in the last 'Type' field.
--
-- If the GADT is declared using the ordinary syntax, e.g.
--
-- >data Ty where
-- >  TCon :: Int -> Bool -> Ty
--
-- then @'Maybe' ['FieldDecl' l]@ is 'Nothing', and the whole constructor's
-- type (such as @Int -> Bool -> Ty@) is stored in the last 'Type' field.  
data GadtDecl a
  = GadtDecl { gdName :: Name a
             , gdFields :: ASTMaybe (ASTList FieldDecl) a
             , gdType :: Type a
             , gdInfo :: a
             }
         
-- | A list of functional dependencies: @ ... | a -> b, c -> d @         
data FunDeps a
  = FunDeps { funDeps :: ASTList FunDep a
            , funDepsInfo :: a
            }
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep a
  = FunDep { funDepLhs :: ASTList Name a
           , funDepRhs :: ASTList Name a
           , funDepInfo :: a
           }
           
-- | The list of declarations that can appear in a typeclass
data ClassBody a
  = ClassBody { cbElements :: ASTList ClassElement a
              , cbInfo :: a
              }
              
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
       
-- | Recognised overlaps for overlap pragmas.       
data OverlapPragma a
  = EnableOverlap { overlapInfo :: a } -- ^ NO_OVERLAP pragma
  | DisableOverlap { overlapInfo :: a } -- ^ OVERLAP pragma
  | IncoherentOverlap { overlapInfo :: a } -- ^ INCOHERENT pragma

data InstBody a
  = InstBody { instBodyDecls :: ASTList InstanceDecl a
                 , instBodyInfo :: a
                 }

-- | Declarations inside an instance declaration.
data InstBodyDecl a
  = InstBodyNormalDecl { instBodyDeclFunbind :: FunBind a
                       , instBodyDeclInfo :: a
                       } -- ^ a normal declaration (@ f x = 12 @)
  | InstBodyTypeDecl { instBodyLhsType :: Type a
                     , instBodyRhsType :: Type a
                     , instBodyDeclInfo :: a
                     } -- ^ an associated type definition (@ type A X = B @)
  | InstBodyDataDecl { instBodyDataNew :: DataOrNewKeyword a
                     , instBodyLhsType :: Type a
                     , instBodyDataCons :: ASTList ConDecl a
                     , instBodyDerivings :: ASTMaybe Deriving a
                     , instBodyDeclInfo :: a
                     } -- ^ an associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { instBodyDataNew :: DataOrNewKeyword a
                         , instBodyLhsType :: Type a
                         , instBodyDataKind :: ASTMaybe Kind a
                         , instBodyDataCons :: ASTList GadtDecl a
                         , instBodyDerivings :: ASTMaybe Deriving a
                         , instBodyDeclInfo :: a
                         } -- ^ an associated data type implemented using GADT style

-- | Function binding
data FunBind a
  = FunBind { funBindMatches :: ASTList Match a 
            , funBindInfo :: a
            }            
             
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
  
-- | Haskell expressions
data Expr a
  = Var { exprName :: Name a
        , exprInfo :: a
        } -- ^ a variable (@ a @)
  | ImplicitVar { exprName :: ImplicitName a
                , exprInfo :: a
                } -- ^ an implicit parameter (@ ?a @)
  | Con { exprName :: Name a
        , exprInfo :: a
        } -- ^ data constructor (@Point@ in @Point 1 2@)
  | Lit { exprLit :: Literal a
        , exprInfo :: a
        } -- ^ primitive literal
  | InfixApp { exprLhs :: Expr a
             , exprOperator :: Name a
             , exprRhs :: Expr a
             , exprInfo :: a
             } -- ^ Infix operator application (@ a + b @)
  | App { exprFun :: Expr a
        , exprArg :: Expr a
        , exprInfo :: a
        } -- ^ Function application (@ f 4 @)
  -- unary minus omitted
  | Lambda { exprBindings :: ASTList Pattern a -- ^ at least one
           , exprInner :: Expr a
           , exprInfo :: a
           } -- ^ lambda expression (@ \a b -> a + b @)
  | Let { exprFunBind :: ASTList FunBind a
        , 