{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Olog.Olog2Spec where

import Data.Either (isRight)
import Math.Agate.Olog.Olog2 (Arrow (..), Path (..), identityPath, toPath, 
    (~), PathException(..), Relator(..), (===))
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)
import GHC.Generics (Selector)
import Control.Exception
import System.Exit
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity

olog2Tests :: TestTree
olog2Tests =
  testGroup "The Olog DSL" [
    testGroup "Basic properties of arrows" [
        testCase "Can create arrows" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 1 2
            in do
                arrow.source @?= 1
                arrow.target @?= 2
        ,
        testCase "an arrow is a path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 1 2
                path :: Path Int
                path = toPath arrow
            in do
                path.source @?= 1
                path.target @?= 2
                path.arrows @?= [ arrow ]
        ,
        testCase "identity path" $
            let
                path :: Path Int
                path = identityPath 1
            in do
                path.source @?= 1
                path.target @?= 1
                path.arrows @?= []
        ,
        testCase "binary compound path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                arrow2 :: Arrow Int
                arrow2 = Arrow "arrow2" 1 2
                path :: Path Int
                path = arrow ~ arrow2
            in do
                path.source @?= 1
                path.target @?= 3
                path.arrows @?= [arrow, arrow2]
        ,
        testCase "multiple compound path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                arrow2 :: Arrow Int
                arrow2 = Arrow "arrow2" 1 2
                arrow3 :: Arrow Int
                arrow3 = Arrow "arrow3" 0 1
                path :: Path Int
                path = arrow ~ arrow2 ~ arrow3
            in do
                path.source @?= 0
                path.target @?= 3
                path.arrows @?= [arrow, arrow2, arrow3]
        ,
        testCase "non-matching compound path fails" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                arrow2 :: Arrow Int
                arrow2 = Arrow "arrow2" 0 1
            in do
                checkFails $ arrow ~ arrow2
        ,
        testCase "valid composition with an identity path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                idPath :: Path Int
                idPath = identityPath 3
                compoundPath = idPath ~ arrow
            in do
                compoundPath.source @?= 2
                compoundPath.target @?= 3
                compoundPath.arrows @?= [arrow]
        ,
        testCase "invalid composition with an identity path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                idPath :: Path Int
                idPath = identityPath 0
            in do
                checkFails $ idPath ~ arrow
        ,
        testCase "valid composition with a differently specified identity path" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 2 3
                compoundPath = Identity 3 ~ arrow
            in do
                compoundPath.source @?= 2
                compoundPath.target @?= 3
                compoundPath.arrows @?= [arrow]
        ],
    testGroup "Basic properties of relators" [
        testCase "Can create relators" $
            let arrow :: Arrow Int
                arrow = Arrow "arrow" 1 2
                arrow2 :: Arrow Int
                arrow2 = Arrow "arrow2" 1 2
                relator :: Relator Int
                relator = arrow === arrow2
            in do
                relator.lhs @?= toPath arrow
                relator.rhs @?= toPath arrow2,
        testCase "Disallow trivial relators" $
            let idPath :: Path Int
                idPath = identityPath (3 :: Int)
            in do
                checkFails $ idPath === idPath
    ]
  ]

checkFails:: (Show a) => a -> Assertion
checkFails block = do
    res :: (Either SomeException a) <- try (evaluate block)
    case res of
        Left _ -> assertBool "" True
        Right unexpectedWin -> assertFailure $
            "expected exception, but successfully got "  ++ show unexpectedWin
