{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Refactor.Parser.Base where

import SourceCode.InfoTypes
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Lens
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

name :: HaskellParser (Name BI)
name = withInfo $ Name <$> astMany (try (qualifier <* symbol ".")) <*> simpleName

simpleName :: HaskellParser (SimpleName BI)
simpleName = withInfo $ SimpleName <$> ((maybe id (++) <$> optionMaybe (symbol "?" <|> symbol "%")) 
                                          <*> many1 (satisfy isIdent))
                          <|> SimpleName <$> many1 (satisfy isHSymbol)

qualifier :: HaskellParser (SimpleName BI)
qualifier = withInfo $ SimpleName <$> many1 (satisfy isIdent)
                          
isIdent, isHSymbol, isPragmaChar :: Char -> Bool
isIdent c = isAlphaNum c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

isPragmaChar c = isAlphaNum c || c == '_'

-- * Lexical analysis

whole :: HaskellParser a -> HaskellParser a
whole p = do res <- p
             whiteSpace
             eof
             return res

symbol :: String -> HaskellParser String
symbol = lexeme . string

lexeme :: HaskellParser a -> HaskellParser a
lexeme p = whiteSpace >> p

whiteSpace :: HaskellParser ()
whiteSpace = void $ many (oneOf " \n\t")

parens :: HaskellParser a -> HaskellParser a
parens p = symbol "(" *> p <* symbol ")"

comma :: HaskellParser ()
comma = void $ symbol ","

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