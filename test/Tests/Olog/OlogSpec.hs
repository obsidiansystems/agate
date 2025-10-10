{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Olog.OlogSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.PetriNet (exampleSIRODE)
import Math.Agate.Olog.Olog(Arc(..), Identity(..), Olog(..), makeOlog, MakeOlogError(..))
import Data.Either (isRight)

type MaybeOlog = Either (MakeOlogError Int) (Olog Int)

ologTests :: TestTree
ologTests =
    testGroup "Ologs basic properties" [ 
        testGroup "sanity tests for arcs" [
            testCase "arcs must have a source of a known dot" $
                let badOlog :: MaybeOlog
                    badOlog =
                        makeOlog [1] [("source", 0, 1)] []
                in badOlog @?= Left (UnknownSource "source" 0),
            testCase "arcs must have a target of a known dot" $
                let badOlog :: MaybeOlog
                    badOlog =
                        makeOlog [0] [("source", 0, 1)] []
                in badOlog @?= Left (UnknownTarget "source" 1),
            testCase "arc is then ok" $
                let goodOlog :: MaybeOlog
                    goodOlog =
                        makeOlog [0, 1] [("y", 1, 0), ("x", 0, 1)] []
                in isRight goodOlog @?= True
        ],
        testGroup "sanity tests for relators" [
            testCase "relators can't just say 1 = 1" $
                let badOlog :: MaybeOlog
                    badOlog =
                        makeOlog [0] [] [([], [])]
                in badOlog @?= Left ForbiddenTrivialIdentity,
            testCase "identities should only use known names" $
                let badOlog :: MaybeOlog
                    badOlog =
                        makeOlog [0] [] [(["identity"], [])]
                in badOlog @?= Left (UnknownArc "identity"),
            testCase "arcs in lhs of identities join up" $
                let badOlog :: MaybeOlog
                    badOlog =
                        makeOlog
                        [0, 1, 2]
                        [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
                        [(["0to1", "1to2"], ["0to2"])]
                in badOlog @?= Left (NonJoiningExpressionLhs ["0to1", "1to2"])
        ]
    ]
    -- it "arcs in rhs of identities join up" $
    --   let badOlog :: MaybeOlog
    --       badOlog =
    --         makeOlog
    --           [0, 1, 2]
    --           [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
    --           [(["0to2"], ["0to1", "1to2"])]
    --    in badOlog `shouldBe` Left (NonJoiningExpressionRhs ["0to1", "1to2"])
    -- it "lhs and rhs of identities have same source and target" $
    --   let badOlog :: MaybeOlog
    --       badOlog =
    --         makeOlog
    --           [0, 1, 2]
    --           [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
    --           [(["1to0", "0to1"], ["1to2", "0to1"])]
    --    in badOlog `shouldBe` Left (IdentityMismatch ["1to0", "0to1"] ["1to2", "0to1"] (0, 0) (0, 2))
    -- it "consistent joined-up identities are ok" $
    --   let goodOlog :: MaybeOlog
    --       goodOlog =
    --         makeOlog
    --           [0, 1, 2]
    --           [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
    --           [(["1to2", "0to1"], ["0to2"])]
    --    in 
    --     case goodOlog of
    --       Left err -> expectationFailure $ show err
    --       Right _ -> pure ()
        -- ]
