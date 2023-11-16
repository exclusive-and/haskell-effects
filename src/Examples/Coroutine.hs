
module Examples.Coroutine where

import Haskell.Effect
import System.IO.Unsafe


-- * The Yield Effect

-- | The yield effect pauses the current computation and returns a value.
data Yield x a where
    Yield :: x -> Yield x ()

-- | Computations that can pause themselves, yield values, and be resumed
-- on-demand by the caller.
type Coroutine x = Eff (Yield x)

-- | Yield a value.
yield :: x -> Coroutine x ()
yield x = send (Yield x)


-- * Interpreting the Yield Effect

-- | The exit states of the coroutine:
-- 
--    * @'Done' r@ means that the coroutine exited normally and returned @r@.
-- 
--    * @'Yielded' x cont@ means that the coroutine was suspended to yield
--      the value @x@. The coroutine can be resumed from where it left off by
--      manually calling @cont@.
data Status r x
    = Done r
    | Yielded x (IO () -> IO (Status r x))

-- TODO: Easier coroutine resumption.
--       Maybe something like @Yielded x (IO () -> IO (Coroutine x r))@.


-- | Run one step of a coroutine and return its exit status.
run :: Coroutine x r -> IO (Status r x)
run (Eff m) = do
    tag <- newContinuation
    delimit tag (Done <$> m (yieldTarget tag))

-- | The 'Yield' effect returns a 'Yielded' exit state.
yieldTarget :: ContEv (Status r x) -> Target (Yield x) (Status r x)
yieldTarget tag = Target tag $ \(Yield x) k -> pure (Yielded x k)

    
-- * Some Fun Yield-related Combinators
    
-- | Lazily collect all the values yielded by a coroutine into a list.
collect :: Coroutine x () -> IO [x]
collect (Eff m) = do
    tag <- newContinuation
    handle tag (Done <$> m (yieldTarget tag))
    where
    handle tag action = unsafeInterleaveIO $ do
        r <- delimit tag action
        case r of
            Done () -> pure []
            Yielded x k -> do
                xs <- handle tag (k (pure ()))
                pure (x : xs)


data These a b = This a | That b | These a b
    deriving Show

-- | Execute two parallel coroutines in lock-step.
-- 
--    * Yields if either coroutine does, including when both do.
--    
--    * The combined coroutine is 'Done' only when both constituents are.
par :: Coroutine x a -> Coroutine y b -> Coroutine (These x y) (a, b)
par (Eff ma) (Eff mb) = Eff $ \(Target tag0 cps) -> do
    tag1 <- newContinuation
    tag2 <- newContinuation
    handle tag0 cps tag1 tag2
        (Done <$> ma (yieldTarget tag1))
        (Done <$> mb (yieldTarget tag2))
    
    where
    handle tag0 cps tag1 tag2 acta actb = do
        ra <- delimit tag1 acta
        rb <- delimit tag2 actb
    
        case (ra, rb) of
            (Done a, Done b) -> pure (a, b)
            
            (Yielded x ka, Done b) -> do
                _ <- control0 tag0 (cps $ Yield $ This x)
                handle tag0 cps tag1 tag2 (ka $ pure ()) (pure $ Done b)
            
            (Done a, Yielded y kb) -> do
                _ <- control0 tag0 (cps $ Yield $ That y)
                handle tag0 cps tag1 tag2 (pure $ Done a) (kb $ pure ())
            
            (Yielded x ka, Yielded y kb) -> do
                _ <- control0 tag0 (cps $ Yield $ These x y)
                handle tag0 cps tag1 tag2 (ka $ pure ()) (kb $ pure ())


-- * A Toy Coroutine

-- | A coroutine version of the Fibonacci algorithm.
fibonacci :: Coroutine Integer ()
fibonacci = fib 0 1
    where
    fib a b = do
        yield a
        fib b (a + b)
