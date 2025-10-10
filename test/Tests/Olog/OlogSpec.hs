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
    testGroup
        "Ologs basic properties" [ 
            testGroup "olog sanity checks" [
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
            ]
        ]
