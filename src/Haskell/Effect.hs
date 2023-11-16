
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE UnboxedTuples  #-}

module Haskell.Effect
    (
{- * Delimited Continuations -}
-- | #continuations#
-- 
-- Haskell's first-class delimited continuation primitives can capture
-- the continuations of running computations as values at runtime.
-- Operationally, 'delimit' and 'control0' can be understood as follows:
-- 
--    * 'delimit' does not do anything on its own; it merely indicates where
--      the sub-computation (and hence continuation) ends.
--    
--    * 'control0' halts the execution of a computation, and captures
--      whatever's left up to the enclosing 'delimit' as a continuation.
-- 
-- 'control0' takes a CPS function as an argument: it will either
-- run the captured continuation, or package it so that it can be run later.
    ContEv, newContinuation,
    delimit,
    CPS, control0,
    
{- * Algebraic Effects -}
-- | #algebraicEffects#
-- 
-- Algebraic effects provide a friendly way of quickly building monadic
-- computations with very abstract side-effects.
-- 
-- Instead of building a new monad from the ground up for each distinct set
-- of effects, algebraic effects systems make use of a canonical monad,
-- in this case the continuation monad, which already knows how to do all
-- of the monad-y things. Relying on a canonical monad means new effects can
-- be defined quickly and easily by:
-- 
--   1. Writing an abstract definition of the desired effects.
-- 
--   2. Providing a concrete interpretation of the abstract effects,
--      so the monad can properly perform them when they occur.
-- 
-- In code, the abstract definition of a set of effects is a GADT with one
-- constructor for each effect. Note that the GADT must have one type
-- parameter. The parameter represents the type of whatever value the effect
-- may return back into the computation.
-- 
-- The interpretations of effects are CPS functions that define what the
-- effect does to the continuation of the computation.
-- A basic state effect might simply read and write in-place, before
-- immediately resuming. Alternatively, an effect like yielding could return
-- the continuation, so that the caller can manually resume it later on.
    Target (..), Eff (..), runEff, send
    ) where

import GHC.Exts
import GHC.IO


-- * Delimited Continuations

-- | Evidence type for identifying continuations.
data ContEv a = ContTag (PromptTag# a)

-- | Create a new continuation ID tag.
newContinuation :: IO (ContEv a)
newContinuation = IO $ \s0 ->
    case newPromptTag# s0 of (# s1, tag #) -> (# s1, ContTag tag #)

-- | See "Haskell.Effect#continuations".
delimit :: ContEv a -> IO a -> IO a
delimit (ContTag tag) (IO m) = IO (prompt# tag m)

-- | Computations in continuation-passing style.
type CPS r a = (IO a -> IO r) -> IO r

-- | See "Haskell.Effect#continuations".
control0 :: ContEv r -> CPS r a -> IO a
control0 (ContTag tag) run = IO (control0# tag continue)
    where
    continue k = case run (\(IO a) -> IO (k a)) of IO r -> r


-- * Algebraic Effects

-- | The canonical monad backing the algebraic effects system.
newtype Eff eff a = Eff (forall r. Target eff r -> IO a)

-- | Run an algebraic effect with its target.
runEff :: Eff eff r -> Target eff r -> IO r
runEff (Eff m) = m

-- | The target of an effect provides the effect's interpretation to the
-- underlying delimited continuation primitives.
data Target eff r where
    Target  :: ContEv r
            -- ^ Which continuation is this effect a part of?
            -> (forall b. eff b -> CPS r b)
            -- ^ The canonical interpretation of the effect.
            -> Target eff r


instance Functor (Eff eff) where
    fmap f (Eff ma) = Eff (\target -> fmap f (ma target))

instance Applicative (Eff eff) where
    pure a = Eff (\_ -> pure a)
    
    Eff mf <*> Eff ma = Eff (\target -> mf target <*> ma target)

instance Monad (Eff eff) where
    return = pure
    
    Eff ma >>= k = Eff $ \target -> do
        a <- ma target
        let Eff mb = k a in mb target


-- | Raise an abstract effect in the algebraic effect monad.
-- Uses the effect's target to provide the canonical interpretation.
send :: eff a -> Eff eff a
send e = Eff $ \(Target tag cps) -> control0 tag (cps e)
