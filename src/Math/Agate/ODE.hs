{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module Math.Agate.ODE (ODESystem (..)) where

-- | First order ODEs
class (Monoid system, Num (Exp system), Num (Field system)) => ODESystem system where
    -- | The type of mathematical expressions associated to the system of ODEs
    type Exp system

    -- | Type type of variables (occurring in those expressions)
    type Var system

    -- The field of an evaluated expression
    type Field system

    -- | Express a basic differential equation of the form var' = expr. The monoid instance for system should add these rates of change together for each given variable.
    (+=) :: Var system -> Exp system -> system

    -- | Contruct the expression which is just a single variable
    var :: Var system -> Exp system

    -- | Evaluates an expression given an assignment of variables
    eval :: (Var system -> Field system) -> Exp system -> Field system

    -- | Lookup the derivative expression for a given variable
    derivative :: system -> Var system -> Exp system

infixl 0 +=
