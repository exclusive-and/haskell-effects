
{- |
   Module       :   Control.Monad.Interrupt

   Copyright    :   (c) 2023-2024, Simon Lovell Bart
   License      :   BSD3 (see the file LICENSE)
   
   Maintainer   :   Simon Lovell Bart <exclusiveandgate@gmail.com>
   Stability    :   experimental
   Portability  :   non-portable (GHC Extensions)

   Monadic operations for approaching control within a computation as a
   first-class concept. There are two parts to this idea:
   
   [Running]
       'delimit' runs a given computation with first-class control
       structures.
   
   [Interrupting]
       'control0' stops the computation in-place, and it uses the
       given function to come up with an appropriate result for 'delimit'
       to return.
-}

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Control.Monad.Interrupt where

import Control.Monad.Primitive
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)


-- Note [Weird monad constraints]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This code uses the following constraint on the monadic operations:
-- 
--      example :: (PrimMonad m, m ~ IO) => ... -> m result
-- 
-- instead of directly naming IO like in:
-- 
--      example :: () => ... -> IO result
-- 
-- This is because the functions are /morally pure/: they are equal to some
-- pure counterparts.
-- 
-- The monad constraint is meant to highlight that spiritually,
-- m /could/ have candidates other than IO if not for the technical
-- artefacts of GHC's primitives.
-- 
-- Moral purity is the same idea that underlies ST.
-- Maybe the GHC primitives can eventually be changed from IO to ST?


-- | 
data ControlTag a = ControlTag (PromptTag# a)

-- | 
newControlTag :: (PrimMonad m, m ~ IO) => m (ControlTag a)
newControlTag = primitive $ \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, ControlTag tag #)


-- | 
delimit :: (PrimMonad m, m ~ IO) => ControlTag a -> m a -> m a
delimit (ControlTag tag) m = primitive (prompt# tag $ internal m)


-- | The type of functions in continuation-passing style (CPS).
type CPS r m a = (m a -> m r) -> m r

-- | 
control0 :: (PrimMonad m, m ~ IO) => ControlTag r -> CPS r m a -> m a
control0 (ControlTag tag) run = primitive (control0# tag continue)
    where
    continue k = internal $ run (\a -> primitive (k $ internal a))
