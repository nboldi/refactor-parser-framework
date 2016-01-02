{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Refactor.Parser.Base where

import SourceCode.InfoTypes
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Lens (view, from)
import Control.Monad.Identity
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.ExtraCombinators
import SourceCode.SourceInfo
import SourceCode.ASTNode
import SourceCode.ASTElems
import SourceCode.Semantics
import qualified SourceCode.TransformInfo as TI
import Text.Parsec
import Data.Char
import qualified Data.Set as Set

import Language.Haskell.Refactor.AST.Base


type BI = NodeInfo BasicInfo ()

type HaskellParser a = ParsecT String () Identity a


-- * Handling source info

-- | Adds node information to a parsed AST node.
-- Composes the source template by cutting out children nodes.
withInfo :: HaskellParser (BI -> b) -> HaskellParser b
withInfo p = do ((res, inp), rng) <- captureSourceRange (captureInputStr p)
                return $ res (NodeInfo (BasicInfo rng inp) ())
                
-- | Orders the parsed AST node to compose it's node information from it's children.
inheritInfo :: HaskellParser (BI -> b) -> HaskellParser b
inheritInfo p = p <*> pure noNodeInfo
        
-- | Replaces the node information in a node with the current node information.
-- Discards node information composed by 'unifyInfo'
withNewInfo :: (ASTNode a BI) => HaskellParser (a BI) -> HaskellParser (a BI)
withNewInfo p = withInfo (p >>= \res -> return $ \inf -> setInfo inf res) 
      
-- * Basic constructs

overlapPragma :: HaskellParser (OverlapPragma BI)
overlapPragma = withInfo 
    $ pragmaBraces (symbol "OVERLAP" *> return EnableOverlap)
  <|> pragmaBraces (symbol "NO_OVERLAP" *> return DisableOverlap)
  <|> pragmaBraces (symbol "OVERLAPPABLE" *> return Overlappable)
  <|> pragmaBraces (symbol "OVERLAPPING" *> return Overlapping)
  <|> pragmaBraces (symbol "OVERLAPS" *> return Overlaps)
  <|> pragmaBraces (symbol "INCOHERENT" *> return IncoherentOverlap)

assoc :: HaskellParser (Assoc BI)
assoc = withInfo 
    $ (symbol "infixr" *> return AssocRight)
  <|> (symbol "infixl" *> return AssocLeft)
  <|> (symbol "infix" *> return AssocNone)
  
name :: HaskellParser (Name BI)
name = withInfo $ lexeme $ Name <$> astMany (try (qualifier <* symbol ".")) <*> simpleName

simpleName :: HaskellParser (SimpleName BI)
simpleName = withInfo $ SimpleName <$> ((maybe id (++) <$> optionMaybe (symbol "?" <|> symbol "%")) 
                                          <*> (many1 (satisfy isIdent) >>= \n -> when (Set.member n reservedNames) (fail "reserved name") >> return n)) 
                          <|> SimpleName <$> (many1 (satisfy isHSymbol) >>= \n -> when (Set.member n reservedOps) (fail "reserved operator") >> return n)

reservedNames = Set.fromList ["_", "class", "data", "default", "deriving", "do", "else", "hiding", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "qualified", "then", "type", "where", "family", "role", "static"]

reservedOps = Set.fromList ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "-", "!"]

qualifier :: HaskellParser (SimpleName BI)
qualifier = withInfo $ SimpleName <$> many1 (satisfy isIdent)
                          
isIdent, isHSymbol, isPragmaChar :: Char -> Bool
isIdent c = isAlphaNum c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

isPragmaChar c = isAlphaNum c || c == '_'

dataOrNew :: HaskellParser (DataOrNewtypeKeyword BI)
dataOrNew = withInfo $ choice 
  [ symbol "data"    *> return DataKeyword
  , symbol "newtype" *> return NewtypeKeyword
  ]   

callConv :: HaskellParser (CallConv BI)
callConv = withInfo $ choice 
  [ symbol "stdcall"    *> pure StdCall
  , symbol "ccall"      *> pure CCall
  , symbol "cplusplus"  *> pure CPlusPlus
  , symbol "dotnet"     *> pure DotNet
  , symbol "jvm"        *> pure Jvm
  , symbol "js"         *> pure Js
  , symbol "javascript" *> pure JavaScript
  , symbol "capi"       *> pure CApi
  ]

safety :: HaskellParser (Safety BI)
safety = withInfo $ choice 
  [ symbol "safe"          *> pure Safe
  , symbol "unsafe"        *> pure Unsafe
  , symbol "threadsafe"    *> pure ThreadSafe
  , symbol "interruptable" *> pure Interruptible
  ]
  
-- * Lexical analysis

whole :: HaskellParser a -> HaskellParser a
whole p = do res <- p
             whiteSpace
             eof
             return res

symbol :: String -> HaskellParser String
symbol = lexeme . string

lexeme :: HaskellParser a -> HaskellParser a
lexeme p = p <* whiteSpace

whiteSpace :: HaskellParser ()
whiteSpace = void $ many (void (oneOf " \n\t") <|> comment)

comment :: HaskellParser ()
comment = void $ 
      try (string "--" <* notFollowedBy (satisfy isHSymbol))
         <* many (noneOf "\n")
  <|> try (string "{-" <* notFollowedBy (char '#'))
         <* manyTill anyChar (string "-}")

parens, pragmaBraces :: HaskellParser a -> HaskellParser a
parens p = symbol "(" *> p <* symbol ")"
pragmaBraces p = symbol "{-#" *> p <* symbol "#-}"

comma :: HaskellParser ()
comma = void $ symbol ","

stringLiteral :: HaskellParser String
stringLiteral = lexeme (char '"' *> many (noneOf "\"") <* char '"')

integer :: HaskellParser Integer
integer = lexeme int

-- integers and naturals
int             = lexeme sign <*> nat

sign            =   (char '-' *> pure negate)
                <|> (char '+' *> pure id)
                <|> pure id

nat             = zeroNumber <|> decimal

zeroNumber      = char '0' *> (hexadecimal <|> octal <|> decimal <|> pure 0)

decimal         = number 10 digit
hexadecimal     = oneOf "xX" *> number 16 hexDigit
octal           = oneOf "oO" *> number 8 octDigit

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }


-- * Helper parsers. Used to parse common AST elements in special ways.
            
astMany :: HaskellParser (e BI) -> HaskellParser (ASTList e BI)
astMany p = (astCons <$> p <*> astMany p) <|> pure astNil

astMany1 :: HaskellParser (e BI) -> HaskellParser (ASTList e BI)
astMany1 p = astCons <$> p <*> astMany p

astSepBy :: HaskellParser (e BI) -> HaskellParser sep -> HaskellParser (ASTList e BI)
astSepBy p sep = astSepBy1 p sep <|> pure astNil

astSepBy1 :: HaskellParser (e BI) -> HaskellParser sep -> HaskellParser (ASTList e BI)
astSepBy1 p sep = astCons <$> p <*> astMany (sep *> p)

astOptionMaybe :: HaskellParser (e BI) -> HaskellParser (ASTMaybe e BI)
astOptionMaybe = (view (from astMaybe) <$>) . optionMaybe