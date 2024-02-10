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
    'catch' and 'delimit' run a given computation that contains first-class
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


catch :: (PrimMonad m, m ~ IO) => m a -> (e -> m a) -> m a
{-# INLINE catch #-}
catch m run = primitive (catch# (internal m) (\e -> internal (run e)))

raise :: (PrimMonad m, m ~ IO) => e -> m a
{-# INLINE raise #-}
raise e = primitive (raiseIO# e)


-- | 
data ControlTag a = ControlTag (PromptTag# a)

-- | 
newControlTag :: forall a m. (PrimMonad m, m ~ IO) => m (ControlTag a)
{-# INLINE newControlTag #-}
newControlTag = primitive $ \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, ControlTag tag #)


-- | 
delimit :: (PrimMonad m, m ~ IO) => ControlTag a -> m a -> m a
{-# INLINE delimit #-}
delimit (ControlTag tag) m = primitive (prompt# tag $ internal m)


-- | The type of functions in continuation-passing style (CPS).
type CPS r m a = (m a -> m r) -> m r

-- | 
control0 :: (PrimMonad m, m ~ IO) => ControlTag r -> CPS r m a -> m a
{-# INLINE control0 #-}
control0 (ControlTag tag) run = primitive (control0# tag continue)
    where
    continue k = internal $ run (\a -> primitive (k $ internal a))
