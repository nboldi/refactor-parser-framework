{-# LANGUAGE CPP, LambdaCase, FlexibleInstances #-}
module Language.Haskell.Refactor.Analyzer where

import Language.Haskell.Refactor.GHC2AST

import GHC
import Outputable
import Outputable
import Bag
import Var
import GHC.Paths ( libdir )
 
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
 
import DynFlags

instance Show (GenLocated SrcSpan AnnotationComment) where
  show = show . unLoc
 
analyze :: String -> IO ()
analyze moduleName = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- don't generate any code
        setSessionDynFlags $ gopt_set (dflags { hscTarget = HscNothing, ghcLink = NoLink }) Opt_KeepRawTokenStream
        target <- guessTarget (moduleName ++ ".hs") Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName moduleName
        p <- parseModule modSum
        t <- typecheckModule p
        
        -- liftIO $ putStrLn $ show $ trfModule $ pm_parsed_source $ tm_parsed_module t
        
        liftIO $ putStrLn "==========="
        
        liftIO $ putStrLn $ show $ fst $ pm_annotations $ tm_parsed_module t
        
        liftIO $ putStrLn $ show $ snd $ pm_annotations $ tm_parsed_module t
        
        -- let mod = pm_parsed_source $ tm_parsed_module t
            -- adtName = msum $ map ((\case TyClD (DataDecl {tcdLName = name}) -> Just (unLoc name); _ -> Nothing) . unLoc) (hsmodDecls (unLoc mod))
        -- case adtName of 
          -- Just name -> liftIO $ putStrLn $ showSDocUnsafe $ ppr $ lookupName name
        
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr $ tm_renamed_source t
        
        -- liftIO $ putStrLn "==========="
        
        -- case tm_renamed_source t of 
          -- Just (renamedMod,_,_,_) -> do
            -- let adtName = msum $ map ((\case DataDecl {tcdLName = name} -> Just (unLoc name); _ -> Nothing) . unLoc) (concatMap group_tyclds $ hs_tyclds renamedMod)
            -- case adtName of 
              -- Just name -> lookupName name >>= liftIO . putStrLn . showSDocUnsafe . ppr . fmap (\(ATyCon tc) -> map varType (tyConTyVars tc))
              -- Nothing -> return ()
        
        -- liftIO $ putStrLn "==========="
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr $ tm_typechecked_source t
        -- liftIO $ putStrLn "==========="
        -- g <- getModuleGraph
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr g
        
     