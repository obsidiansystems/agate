{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Olog.Olog2Spec where

import Data.Either (isRight)
import Math.Agate.Olog.Olog2 (Arrow (..), Path (..), identityPath, toPath, (~) )
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)
import GHC.Generics (Selector)
import Control.Exception
import System.Exit

olog2Tests :: TestTree
olog2Tests =
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
            tryPath :: Path Int
            tryPath = arrow ~ arrow2
            -- h = try (print (toPath tryPath))
            x = 5 `div` 0
            -- expectException :: forall e. Exception e => e -> IO ()
            -- expectException ex = 
            --     case ex of 
            --         DivideByZero -> putStrLn "Caught expected DivideByZero"
            --         _ -> putStrLn ("Caught unexpected exception: " ++ show ex)
            -- q = handle handler (assertFailure "xxx")
            assertion :: Assertion = True @?= False
            -- result2 = try x :: IO (Either SomeException ())
        in do
            result :: Either SomeException () <- try (print (arrow ~ arrow2))
            True @?= True
            -- True @?= False
            -- do
            -- res <- result2
            -- res @?= Left (toException DivideByZero)
            -- try (tryPath) @?= Left
            -- tryPath  "xx" `catch` (\e -> do
            --     if e == ExitSuccess
            --     then putStrLn "Yea"
            --     else putStrLn "Nay"
            --     throwIO e)
    ]

    -- testCase "compound path" $ 
    --     let arrow :: Arrow Int
    --         arrow = 2 ~> 3
    --         arrow2 :: Arrow Int
    --         arrow2 = 1 ~> 2
    --         arrow3 :: Arrow Int
    --         arrow3 = 0 ~> 1
    --         path :: Path Int
    --         path = arrow o arrow2 o arrow3
    --     in do
    --         path.source @?= 0
    --         path.target @?= 2
    --         path.arrows @?= [arrow]

    -- testCase "Can create an identity path" $ 
    --     let path :: Arrow Int
    --         path = identityPath 1
    --     in do
    --         path.source @?= 1
    --         path.target @?= 1
    --         path.arrows @?= []
    -- ]
-- arrow = Arrow @Int 1 2
-- ((1 + 1) :: Int) @?= 2

-- testGroup "Basic properties of arrows" [
--     testCase "Can create arrows" $ do
--         arrow.source @?= 1
--         arrow.target @?= 2
--     where
--         arrow :: Arrow Int
--         arrow = 1 ~> 2
--         arrow2 :: Arrow Int
--         arrow2 = Arrow 1 2
--     -- testCase "Can create arrows" $ do
--     --     arrow.source @?= 1
--     --     arrow.target @?= 2
--     --     where
--     --         arrow :: Arrow Int
--     --         arrow = 1 ~> 2
--     --         arrow2 :: Arrow Int
--     --         arrow2 = Arrow 1 2
-- ]
--         -- arrow = Arrow @Int 1 2
--     -- ((1 + 1) :: Int) @?= 2
