module Math.Agate.PetriNet where

import Math.Agate.ODESystem

class Monoid net => PetriNet net where
  type Place net
  type Transition net
  -- | Express a basic building block Petri net that consists of a single transition with given input and output places.
  -- These can then be combined with the monoid instance to build larger nets.
  transition :: [Place net] -> Transition net -> [Place net] -> net

newtype AsODE system = AsODE { asODE :: system }
  deriving newtype (Semigroup, Monoid, Show)

instance forall system. ODESystem system => PetriNet (AsODE system) where
  type Place (AsODE system) = Var system
  type Transition (AsODE system) = Exp system
  transition inputs rate outputs = AsODE $
       mconcat [i += (- rate) * product (map (var @system) outputs) | i <- inputs]
    <> mconcat [o += rate * product (map (var @system) inputs)      | o <- outputs]

exampleSIR :: (Fractional (Transition net), PetriNet net) => (String -> Place net) -> Transition net -> Transition net -> net
exampleSIR place recovery transmission =
  mconcat
    [ transition [place "I", place "S"] transmission [place "I", place "I"]
    , transition [place "I"] recovery [place "R"]
    ]

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE $ exampleSIR id 0.02 0.1
