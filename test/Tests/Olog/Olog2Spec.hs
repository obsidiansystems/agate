{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Olog.Olog2Spec where

import Data.Either (isRight)
import Math.Agate.Olog.Olog2 (Arrow (..), Path (..), (~>), identityPath, toPath, (~) )
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)

olog2Tests :: TestTree
olog2Tests =
  testGroup "Basic properties of arrows" [
    testCase "Can create arrows" $ 
        let arrow :: Arrow Int
            arrow = 1 ~> 2
            arrow2 :: Arrow Int
            arrow2 = Arrow 1 2
        in do
            arrow.source @?= 1
            arrow.target @?= 2
    ,
    testCase "an arrow is a path" $ 
        let arrow :: Arrow Int
            arrow = 1 ~> 2
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
    testCase "compound path" $ 
        let arrow :: Arrow Int
            arrow = 2 ~> 3
            arrow2 :: Arrow Int
            arrow2 = 1 ~> 2
            arrow3 :: Arrow Int
            arrow3 = 0 ~> 1
            path :: Path Int
            path = arrow ~ arrow2 ~ arrow3
        in do
            path.source @?= 0
            path.target @?= 3
            path.arrows @?= [arrow, arrow2, arrow3]

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
    ]
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
