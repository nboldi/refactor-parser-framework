{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}

-- | The purpose of this module is to transform an AST with basic infos to an AST with templates.
module SourceCode.TransformInfo (transformSourceInfo) where

import SourceCode.Semantics
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Data.SmartTrav.Class
import Data.Maybe
import Text.Parsec.PosOps
import SourceCode.RangeTree
import SourceCode.SourceNotation
import SourceCode.SourceTree
import SourceCode.ToSourceTree
import SourceCode.InfoTypes
import Debug.Trace

type BI sema = NodeInfo BasicInfo sema
type NI sema = NodeInfo TemplateInfo sema
        
        
-- | Creates source templates from simple source strings
transformSourceInfo :: (SemanticInfo sema, SmartTrav node, ToSourceRose node (BI sema))
                    => node (BI sema) -> node (NI sema)
transformSourceInfo = cutOutTemplates . expandNodes

-- | Expands nodes to contain all their children
expandNodes :: forall node sema . SmartTrav node => node (BI sema) -> node (BI sema)
expandNodes n = evalState (expandNodes' n) [Nothing]
  where expandNodes' :: SmartTrav node => node (BI sema) -> State [Maybe (BI sema)] (node (BI sema))
        expandNodes' = smartTrav desc asc f
        
        desc = modify (Nothing:)
        asc  = modify (\case (x : y : xs) -- if both exist union, otherwise keep existing
                                          -> maybe y (\x' -> maybe x (Just . (sourceInfo %~ unifyBasicInfo (x' ^. sourceInfo))) y) x : xs)
        f inf 
          = do ni <- gets head
               let newInfo = maybe inf (\ni -> inf & sourceInfo %~ unifyBasicInfo (ni ^. sourceInfo)) ni
               modify (\case (_ : xs) -> Just newInfo : xs)
               return newInfo

-- | Replaces assigned input of nodes with source templates.
-- Goes top-down and replaces the info in every node according to structure of the whole tree.
cutOutTemplates :: forall node sema . (SemanticInfo sema, SmartTrav node, ToSourceRose node (BI sema)) 
                => node (BI sema) -> node (NI sema)
cutOutTemplates ast 
  = let rose = fmap (view sourceInfo) (toRose ast)
     in evalState (runReaderT (cutOutTemplates' ast)
                              (rose, generateRangeTree Root rose))
                  (Root,0)                 

  where cutOutTemplates' 
          :: node (BI sema) 
          -> ReaderT (SourceRose BasicInfo, [RangeTree]) 
                                                      (State (RootIndex, Int)) 
                                                      (node (NI sema))
        cutOutTemplates' 
          = do smartTrav ( lift $ modify (\(ri, i) -> (RootIndex i ri, 0)) )
                         ( lift $ modify (\(RootIndex i ri, _) -> (ri, i+1)) )
                         createTempInf
        createTempInf bi 
          = do (tree, rngTree) <- ask
               currInd <- gets fst
               let rng = bi ^?! sourceInfo.biRange
                   inp = bi ^?! sourceInfo.biInput
                   relativeIndAndRng ind = (info,rel)
                     where info = fromMaybe (error "No range for a node that is to be cut out")
                                    . getRange 
                                    . roseInfo 
                                    $ resolveRootInd ind tree
                           rel = ind `indRelativelyTo` currInd 
                   contained = findContainedWhere 
                                 (\rng ind -> not (emptyRange rng) 
                                                && not (ind `rootPrefixOf` currInd)) 
                                 rng rngTree
                   toRemove = map relativeIndAndRng contained 
               return $ bi & sourceInfo .~ TemplateInfo (Just rng) 
                                                        (Just $ createTemplate toRemove rng inp)
