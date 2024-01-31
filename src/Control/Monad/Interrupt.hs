
{- |
   Module       :   Control.Monad.Interrupt

   Copyright    :   (c) 2023-2024, Simon Lovell Bart
   License      :   BSD3 (see the file LICENSE)
   
   Maintainer   :   Simon Lovell Bart <exclusiveandgate@gmail.com>
   Stability    :   experimental
   Portability  :   non-portable (GHC Extensions)

   = Monads, Continuations, And Interrupts

   Monads construct computations out of sequences of operations.
   This module defines some meta-operations that can make changes to the
   computation from within, while it is running.
   
   The process happens in two parts, each with its own operation:
   
   [Running]
       'delimit' runs a given computation that can contain the
       sequence-breaking meta-operation.
       If the meta-operation doesn't occur anywhere in the given
       computation, then this does nothing.
   
   [Interrupting]
       'control0' can only be run by 'delimit'.
       Once called, it stops the computation and passes the unfinished part
       of the sequence to a given function. The return value of the given
       function will in turn be returned by 'delimit' as the result of the
       whole computation.
   
   This technique is called /delimited continuations/.
   It has many analogues, including CPU interrupts and exception handlers.
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
