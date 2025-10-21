{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Math.Agate.Algol where

import Data.IORef

class (Monoid system, Num (Exp system)) => Algol system where
    type Exp system
    type Var system

    assign :: Var system -> Exp system -> system
    var :: Var system -> Exp system
    ifThenElse :: Exp system -> system -> system -> system
    while :: Exp system -> system -> system
    new :: (Var system -> system) -> system

newtype AlgolIO v = AlgolIO {runAlgolIO :: IO ()}
    deriving newtype (Semigroup, Monoid)

instance (Num v) => Num (IO v) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger

instance (Num v, Eq v) => Algol (AlgolIO v) where
    type Exp (AlgolIO v) = IO v
    type Var (AlgolIO v) = IORef v

    assign :: IORef v -> IO v -> AlgolIO v
    assign ref expr = AlgolIO $ do
        val <- expr
        writeIORef ref val
    var :: IORef v -> IO v
    var ref = readIORef ref
    ifThenElse :: IO v -> AlgolIO v -> AlgolIO v -> AlgolIO v
    ifThenElse cond (AlgolIO thenBranch) (AlgolIO elseBranch) =
        AlgolIO $ do
            c <- cond
            if c /= 0 then thenBranch else elseBranch
    while :: IO v -> AlgolIO v -> AlgolIO v
    while cond (AlgolIO body) =
        AlgolIO $ do
            c <- cond
            if c /= 0
                then body >> runAlgolIO (while cond (AlgolIO body))
                else return ()
    new :: (IORef v -> AlgolIO v) -> AlgolIO v
    new cont =
        AlgolIO $ do
            ref <- newIORef 0
            runAlgolIO (cont ref)

sampleProgram :: forall system. (Algol system) => Var system -> system
sampleProgram y =
    new $ \x ->
        assign x 5
            <> assign y 0
            <> while
                (var @system x)
                ( (assign x (var @system x - 1))
                    <> (assign y (var @system y + 1))
                )

runIt :: IO ()
runIt =
    do
        y <- newIORef (0 :: Double)
        runAlgolIO (sampleProgram y)
        v <- readIORef y
        print "The value is..."
        print v

-- runIt has to be an IO ()
-- so sampleProgram has to be an AlgolIO v
-- so the implicit Algol system has to be AlgolIO v

-- directions for Idealized Algol:
-- more conveniences: typed variables, lambdas
-- can it be an instance of a monad (not a monoid), with more operations
-- so your IA program can be a do expression
-- can our ODE solvers be IA programs?
-- change the kinds of 'system', their constraints? or a function type => type e.g. List
-- so Algol becomes a Monad => Monad ...? instead of a Monoid => Monoid ...
-- more implementations of Algol, other things you can do with it
