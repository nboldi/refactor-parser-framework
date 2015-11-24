module SourceCode.Semantics where

import SourceCode.SourceInfo
import SourceCode.InfoTypes
import Control.Lens

class SemanticInfo i where
  emptySemaInfo :: i

instance SemanticInfo () where
  emptySemaInfo = ()
  
instance (SourceInfo srci, SemanticInfo sema) => SourceInfo (NodeInfo srci sema) where
  generateInfo anc children 
    = NodeInfo (generateInfo (anc ^. sourceInfo) (children ^.. each.sourceInfo)) emptySemaInfo
  noNodeInfo = NodeInfo noNodeInfo emptySemaInfo
  