{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Refactor.Parser.Base where

import SourceCode.InfoTypes
import Text.Parsec
import Control.Monad.Identity
import Control.Applicative
import Text.Parsec.ExtraCombinators
import SourceCode.SourceInfo
import SourceCode.ASTNode
import SourceCode.Semantics

type BI = NodeInfo BasicInfo ()
type NI = NodeInfo TemplateInfo ()

type HaskellParser a = ParsecT String () Identity a


-- * Handling source info

-- | Adds node information to a parsed AST node.
-- Composes the source template by cutting out children nodes.
withInfo :: HaskellParser (BI -> b) -> HaskellParser b
withInfo p = do ((res, inp), rng) <- captureSourceRange (captureInputStr p)
                ($ NodeInfo (BasicInfo rng inp) ()) <$> p
                
-- | Orders the parsed AST node to compose it's node information from it's children.
inheritInfo :: HaskellParser (BI -> b BI) -> HaskellParser (b BI)
inheritInfo p = p <*> pure noNodeInfo
        
-- | Replaces the node information in a node with the current node information.
-- Discards node information composed by 'unifyInfo'
withNewInfo :: (ASTNode a BI) => HaskellParser (a BI) -> HaskellParser (a BI)
withNewInfo p = withInfo (p >>= \res -> return $ \inf -> setInfo inf res) 
      