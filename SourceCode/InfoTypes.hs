{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module SourceCode.InfoTypes where

import Text.Parsec.PosOps
import SourceCode.SourceInfo
import SourceCode.SourceNotation
import SourceCode.SourceTree

import Data.Data
import Data.Data.Lens
import Control.Lens

-- | Syntactic and semantic information about an AST node
data NodeInfo src sem
  = NodeInfo { _sourceInfo :: src
             , _semanticInfo :: sem
             }      
    deriving (Show, Typeable, Data)             

makeLenses ''NodeInfo
    
-- All source info is considered equal, so in a container all AST nodes 
-- with the same structure are not differentiated.
instance Eq (NodeInfo source sema) where
  _ == _ = True 
instance Ord (NodeInfo source sema) where
  _ `compare` _ = EQ
    
-- | An intermediate node information that contains the source range 
-- of the node and the remaining input of the parser when it started 
-- parsing the element. We could only store the text from which the node
-- was parsed, but then we could not extend nodes from their children.
data BasicInfo -- ^ The node was originally in the source 
  = BasicInfo { _biRange :: SourceRange
              , _biInput :: String
              }
  | InheritInfo
  deriving (Eq, Ord, Typeable, Data)
  
$(makeLenses ''BasicInfo)
  
instance SourceInfo BasicInfo where
  generateInfo ancestor []
    = ancestor & biRange %~ rngStartAsRange
  generateInfo _ children
    = foldl1 unifyBasicInfo children
  noNodeInfo = InheritInfo
  
deriving instance Typeable SourceTemplateElem
deriving instance Data SourceTemplateElem
deriving instance Typeable NodeIndex
deriving instance Data NodeIndex

-- | Gets the range of a basic info
getRange :: BasicInfo -> Maybe SourceRange
getRange = preview biRange

-- | Combines two basic infos into one
unifyBasicInfo :: BasicInfo -> BasicInfo -> BasicInfo
unifyBasicInfo (BasicInfo rng1 inp1) (BasicInfo rng2 inp2) 
  = BasicInfo (rng1 `srcRngUnion` rng2) 
              (if srcRangeBegin rng1 < srcRangeBegin rng2 then inp1 else inp2)
unifyBasicInfo bi InheritInfo = bi
unifyBasicInfo InheritInfo bi = bi

instance Show BasicInfo where
  show (BasicInfo rng inp) 
    = shortShowRng rng ++ " '" ++ takeSourceRange (rngFromStart rng) inp ++ "'"
  show InheritInfo = "<inherited>"
      
-- | The final meta information that can be used to pretty print the AST
data TemplateInfo 
  = TemplateInfo 
      { _niRange     :: Maybe SourceRange
      , _niTemplate  :: Maybe SourceTemplate 
      }
      -- [Comment]    -- ^ comments belonging
      -- [Attribute]  -- ^ attributes
         deriving (Eq, Ord, Typeable, Data)
  
$(makeLenses ''TemplateInfo)

  
instance SourceInfo TemplateInfo where
  generateInfo ancestor [] 
    = TemplateInfo (rngStartAsRange <$> view niRange ancestor) (Just [])
  generateInfo _ infs
    = TemplateInfo (foldl1 (\a b -> srcRngUnion <$> a <*> b) (infs ^.. traverse.niRange)) 
                   (Just (map (\i -> NodeElem (NthChildOf i Current)) 
                              (take (length infs) [0..])))
  noNodeInfo = TemplateInfo Nothing Nothing
  
instance Show TemplateInfo where
  show (TemplateInfo rng templ) = maybe "" shortShowRng rng ++ "||" ++ maybe "" (concatMap show) templ ++ "||"
      
nodeTemplate :: TemplateInfo -> SourceTemplate
nodeTemplate (TemplateInfo { _niTemplate = Just t }) = t
  