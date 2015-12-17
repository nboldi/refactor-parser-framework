{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Refactor where

import Test.HUnit hiding (test)
import System.Timeout
import Data.Maybe
import Data.Function
import Data.Either.Combinators
import Text.Parsec
import Text.Parsec.Error

import Language.Haskell.Refactor.Parser.Base
import SourceCode.ToSourceTree
import SourceCode.InfoTypes
import SourceCode.PrettyPrint
import qualified SourceCode.TransformInfo as TI
import Data.SmartTrav

test = tests >>= runTestTT
  
tests = do progTests <- programTests 
           return $ TestList 
                      [ TestLabel "nameTests" nameTests
                      ]
                      
programTests = return []

nameTests = TestList $ map (\n -> TestLabel n $ TestCase (assertParsedOk name n)) names

names = [ "A.B.almafa", "A.B.+", "almafa", "?implicit", "%linearImplicit", "a'", "a_b_", "+", "++", ":!:" ]


-- * Helper functions
  
equallyPrinted :: (ToSourceRose a BI, ToSourceRose a TemplateInfo, SmartTrav a, Functor a) 
               => String -> a BI -> Maybe String
equallyPrinted s a = let ppres = (prettyPrint . TI.transformSourceInfo) a
  in if ppres == s then Nothing 
                   else Just $ "The result of pretty printing is `" ++ ppres ++ "`"
  
defaultTimeoutMSecs = 100000

assertParsedOk = assertParsedOkCustom defaultTimeoutMSecs "(test)"
assertParsedAST = assertParsedASTCustom defaultTimeoutMSecs "(test)"

-- | Protect tests from infinite loops
withTimeout :: Int -> IO a -> IO a
withTimeout msecs comp
  = do res <- timeout msecs comp 
       return $ fromMaybe (error $ "Did not terminate in " 
                                      ++ show (fromIntegral msecs / 100000.0 :: Double) ++ " seconds.") res

-- | Assert that parse is successful
assertParsedOkCustom :: Int -> String -> HaskellParser a -> String -> Assertion
assertParsedOkCustom msecs srcname
  = assertParsedASTCustom msecs srcname (\_ _ -> Nothing)
         
-- | Assert parse success and check the AST with a function that returns just an error message
-- when the AST is not correct.
assertParsedASTCustom :: Int -> String -> (String -> a -> Maybe String) -> HaskellParser a -> String -> Assertion
assertParsedASTCustom msecs srcname validate parser source 
  = withTimeout msecs $ 
      do let res = runParser (whole parser) () srcname source
         assertBool ("'" ++ source ++ "' was not accepted: " ++ show (fromLeft' res)) (isRight res)
         case validate source $ fromRight' res of 
           Just err -> assertFailure ("'" ++ source ++ "' was not correct: " ++ err)
           Nothing -> return ()
        
-- | Assert that the parse fails
assertSyntaxError :: (Show a) => HaskellParser a -> String -> (String -> Bool) -> Assertion 
assertSyntaxError = assertSyntaxErrorTimeout defaultTimeoutMSecs 
               
assertSyntaxErrorTimeout :: (Show a) 
  => Int -> HaskellParser a -> String -> (String -> Bool) -> Assertion
assertSyntaxErrorTimeout msecs parser source failMess 
  = withTimeout msecs $
      do let res = runParser (whole parser) () "(test)" source
         case res of       
           Left pErr -> case errorMessages pErr of 
             err:_ -> assertBool ("`" ++ source ++ "` should fail with a correct message. Failed with: " ++ messageString err) 
                        (failMess (messageString err))
             [] -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. It failed without a message"
           Right val -> assertFailure $ "`" ++ source ++ "` should fail with a correct message. Parsed: " ++ show val
  
-- | Assert that two ASTs are structurally equivalent
eqAST :: (Functor a, Eq (a ())) => a b -> a b -> Bool
eqAST = (==) `on` fmap (const ())
  
-- | Parses two inputs with the same parser and compares the results
assertParsedSame :: (Eq a, Show a) => (a -> a -> Bool) -> HaskellParser a -> String -> String -> Assertion
assertParsedSame = assertParsedSameTimeout defaultTimeoutMSecs
  
assertParsedSameTimeout :: (Eq a, Show a) => Int -> (a -> a -> Bool) -> HaskellParser a -> String -> String -> Assertion
assertParsedSameTimeout msecs isSameAs parser s1 s2 
  = withTimeout msecs $ 
      do let parseRes1 = runParser (whole parser) () "(test)" s1
         let parseRes2 = runParser (whole parser) () "(test)" s2
         case (parseRes1,parseRes2) of
           (Right result1, Right result2) ->  
                    assertBool ("Parse results from `" ++ s1 ++ "` and `" ++ s2 ++ "` are not the same" )
                                  (result1 `isSameAs` result2)
           (Left err, _) -> assertFailure $ "Paring of `" ++ s1 ++ "` failed with error: " ++ show err
           (_, Left err) -> assertFailure $ "Paring of `" ++ s2 ++ "` failed with error: " ++ show err
  
  