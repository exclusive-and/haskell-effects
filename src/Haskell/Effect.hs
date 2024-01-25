
-- | Module         :   Haskell.Effect
--   Description    :   Monadic Algebraic Effects in Haskell
--   
--   Copyright      :   (c) 2023-2024, Simon Lovell Bart
--   License        :   BSD3 (see the file LICENSE)
--   Maintainer     :   Simon Lovell Bart <exclusiveandgate@gmail.com>
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

{-# LANGUAGE RankNTypes #-}

module Haskell.Effect where

import Haskell.Interrupt

-- TODO: Update all these docs


-- | The target of an effect provides the effect's interpretation to the
-- underlying delimited continuation primitives.
data Target eff r where
    Target  :: ControlTag r
            -- ^ Which continuation is this effect a part of?
            -> (forall b. eff b -> CPS r IO b)
            -- ^ The canonical interpretation of the effect.
            -> Target eff r


-- | The canonical monad backing the algebraic effects system.
newtype Eff eff a = Eff (forall r. Target eff r -> IO a)

-- | Run an algebraic effect with its target.
runEff :: Eff eff r -> Target eff r -> IO r
runEff (Eff m) = m


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
