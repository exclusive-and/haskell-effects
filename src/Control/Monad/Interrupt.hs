{- |
Module          :   Control.Monad.Interrupt

Copyright       :   (c) 2023-2024, Simon Lovell Bart
License         :   BSD3 (see the file LICENSE)
Maintainer      :   Simon Lovell Bart <exclusiveandgate@gmail.com>
Stability       :   experimental
Portability     :   non-portable (GHC Extensions)

Monadic operations where control is first-class data.
There are two parts to this idea:

[Running]
    'catch' and 'delimit' run computations that can contain first-class
    control operations.

[Interrupting]
    'raise' and 'control0' override the main execution plan, forcing
    control to return immediately to the delimiter running the computation.
-}

{-# LANGUAGE MagicHash #-}

module Control.Monad.Interrupt where

import Control.Monad.Primitive
import GHC.Exts (
    catch#, raiseIO#,
    PromptTag#, newPromptTag#, prompt#, control0#
    )


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


-- * Catch and Raise

-- | Catch exceptions raised when the given computation is evaluated.
-- 
--  [Running]
--      Transparent if no exception was raised.
--
--  [Interrupting]
--      Computes an alternate result using an exception handler.
catch :: (PrimMonad m, m ~ IO) => m a -> (e -> m a) -> m a
{-# INLINE catch #-}
catch m run = primitive (catch# (internal m) (\e -> internal (run e)))

-- | Raise an exception in a computation. Forces control to return to the
-- nearest enclosing 'catch'.
raise :: (PrimMonad m, m ~ IO) => e -> m a
{-# INLINE raise #-}
raise e = primitive (raiseIO# e)


-- * Delimit and Control0

-- | 'ControlTag's explicitly match 'control0's with 'delimit's.
-- 
-- The explicit pairing helps in two ways:
-- 
--   1. It allows control operations to unambiguously choose where to send
--      control, so that many of them can be nested without conflicts.
--   
--   2. It communicates to 'control0' what type of exit value to produce.
data ControlTag a = ControlTag (PromptTag# a)

-- | Create a new control tag.
newControlTag :: forall a m. (PrimMonad m, m ~ IO) => m (ControlTag a)
{-# INLINE newControlTag #-}
newControlTag = primitive $ \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, ControlTag tag #)


-- | Catch delimited control operations triggered when the given
-- computation is evaluated.
-- 
--  [Running]
--      Transparent if no control operations occurred.
--  
--  [Interrupting]
--      Computes an alternate result using the continuation and CPS function
--      sent from 'control0'.
delimit :: (PrimMonad m, m ~ IO) => ControlTag a -> m a -> m a
{-# INLINE delimit #-}
delimit (ControlTag tag) m = primitive (prompt# tag $ internal m)


-- | The type of functions in continuation-passing style (CPS).
type CPS r m a = (m a -> m r) -> m r

-- | Trigger a delimited control operation.
-- Sends control to the correspondingly tagged enclosing 'delimit', along
-- with the remaining continuation of the computation.
-- 
-- = control0 does not capture all of IO!
-- 
-- The constraint distinguishing 'IO' apart from monads that incidentally
-- /happen to be equal to/ 'IO' really matters here!
-- There are 'IO' actions that break completely when code is re-entrant,
-- and so extra care is required when pairing those with delimited control.
--
-- There are no restrictions around how many times the continuation gets
-- called, which wreaks havoc if the continuation has code that expects to
-- only run once:
-- 
--    - Zero calls to the continuation can cause resources to never be
--      freed, thus causing leaks and access conflicts.
--
--    - More than one call can see the first call free a captured resource,
--      only for the second call to unknowingly have a dead reference.
-- 
-- At present, the only mitigation is for the caller to be aware of this
-- problem, and for them to ensure that such delicate computations are only
-- resumed exactly once.
control0 :: (PrimMonad m, m ~ IO) => ControlTag r -> CPS r m a -> m a
{-# INLINE control0 #-}
control0 (ControlTag tag) run = primitive (control0# tag continue)
    where
    continue k = internal $ run (\a -> primitive (k $ internal a))
