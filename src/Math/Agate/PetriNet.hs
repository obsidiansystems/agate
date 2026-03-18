module Math.Agate.PetriNet (PetriNet (..), AsODE (..), PetriNetImpl (..), incidenceMatrix, pInvariants) where

import Data.List (transpose)
import Data.Map.Lazy qualified as M
import Data.Ratio (denominator, numerator)
import Data.Set qualified as S
import Math.Algebra.LinearAlgebra (kernel)
import Math.Agate.ODE
import Prelude hiding (id)

class (Monoid net) => PetriNet net where
    type Place net
    type Transition net

    {- | Express a basic building block Petri net that consists of a single transition with given input and output places.
    These can then be combined with the monoid instance to build larger nets.
    -}
    transition :: [Place net] -> Transition net -> [Place net] -> net

newtype AsODE system = AsODE {asODE :: system}
    deriving newtype (Semigroup, Monoid, Show)

instance forall system. (ODESystem system) => PetriNet (AsODE system) where
    type Place (AsODE system) = Var system
    type Transition (AsODE system) = Exp system
    transition inputs rate outputs =
        AsODE $
            mconcat [i += (-rate) * product (map (var @system) inputs) | i <- inputs]
                <> mconcat [o += rate * product (map (var @system) inputs) | o <- outputs]

type TransId = Int
data PetriNetImpl p t = PetriNetImpl
    { numTransitions :: Int
    , transitions :: M.Map TransId t
    , places :: S.Set p
    , placeToTransitions :: M.Map (p, TransId) Int
    , transitionToPlaces :: M.Map (TransId, p) Int
    }
    deriving (Show)

instance (Ord p, Ord t) => Semigroup (PetriNetImpl p t) where
    p1 <> p2 =
        PetriNetImpl
            { numTransitions = numP1 + numP2
            , transitions = foldMap transitions [p1', p2']
            , places = foldMap places [p1', p2']
            , placeToTransitions = foldMap placeToTransitions [p1', p2']
            , transitionToPlaces = foldMap transitionToPlaces [p1', p2']
            }
      where
        (numP1, numP2) = (numTransitions p1, numTransitions p2)
        p1' = p1
        p2' =
            p2
                { transitions = (numP1 +) `M.mapKeys` transitions p2
                , placeToTransitions = (\(p, id) -> (p, id + numP1)) `M.mapKeys` placeToTransitions p2
                , transitionToPlaces = (\(id, p) -> (id + numP1, p)) `M.mapKeys` transitionToPlaces p2
                }

instance (Ord p, Ord t) => Monoid (PetriNetImpl p t) where
    mempty = PetriNetImpl 0 M.empty S.empty M.empty M.empty

instance (Ord p, Ord t) => PetriNet (PetriNetImpl p t) where
    type Place (PetriNetImpl p t) = p
    type Transition (PetriNetImpl p t) = t

    transition sources trans targets =
        PetriNetImpl
            { numTransitions = 1
            , places = S.fromList $ sources ++ targets
            , transitions = M.singleton 0 trans
            , placeToTransitions = M.fromListWith (+) [(pair, 1) | pair <- (,0) <$> sources]
            , transitionToPlaces = M.fromListWith (+) [(pair, 1) | pair <- (0,) <$> targets]
            }

-- | The incidence matrix C where C[p,t] = (output arcs from t to p) - (input arcs from p to t).
-- Rows are places (in Enum order), columns are transitions (by TransId).
incidenceMatrix :: (Bounded p, Enum p, Ord p) => PetriNetImpl p t -> [[Int]]
incidenceMatrix net =
    [ [ M.findWithDefault 0 (tId, p) (transitionToPlaces net)
      - M.findWithDefault 0 (p, tId) (placeToTransitions net)
      | tId <- [0 .. numTransitions net - 1]
      ]
    | p <- [minBound .. maxBound]
    ]

-- | Compute a basis of integer P-invariants: vectors y such that y · C = 0.
-- A P-invariant certifies that the weighted sum y · marking is conserved under any firing.
pInvariants :: (Bounded p, Enum p, Ord p) => PetriNetImpl p t -> [[Int]]
pInvariants =
    map clearDenominators
    . kernel
    . transpose
    . map (map fromIntegral)
    . incidenceMatrix

clearDenominators :: [Rational] -> [Int]
clearDenominators xs =
    let l = foldl lcm 1 (map denominator xs)
        ints = [fromIntegral (numerator x * (l `div` denominator x)) | x <- xs]
        g = foldl gcd 0 (map abs ints)
        result = if g == 0 then ints else map (`div` g) ints
    in  case dropWhile (== 0) result of
            (x : _) | x < 0 -> map negate result
            _ -> result
