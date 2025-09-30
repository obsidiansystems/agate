{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Math.Agate.Algol where
import Math.Agate.ODESystem (PolynomialODE)
import Math.Algebras.Commutative (GlexPoly)
import Data.IORef
import Control.Applicative (liftA2)

-- import Data.Map (Map)
-- import Data.Map qualified as Map

class (Monoid system, Num (Exp system)) => Algol system where
    type Exp system
    type Var system

    assign :: Var system -> Exp system -> system
    var :: Var system -> Exp system
    ifThenElse :: Exp system -> system -> system -> system
    while :: Exp system -> system -> system
    new :: (Var system -> system) -> system

newtype AlgolIO v = AlgolIO { runAlgolIO :: IO () }
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

sampleProgram :: forall system . Algol system => Var system -> system
sampleProgram y =
    new $ \x ->
        assign x 5 <> assign y 0 <> 
            while (var @system x) (
                ( assign x (var @system x - 1)) <>
                ( assign y (var @system y  + 1))
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