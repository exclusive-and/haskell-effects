
-- | Module         :   Haskell.Interrupt
--   Description    :   Categorical Semantics of Interrupt Operations
--
--   Copyright      :   (c) 2023-2024, Simon Lovell Bart
--   License        :   BSD3 (see the file LICENSE)
--   Maintainer     :   Simon Lovell Bart <exclusiveandgate@gmail.com>
--
-- In constructive logic, computers are characterized by Kleisli
-- categories. These categories have the higher order '>>=' combinator to
-- compose small operations together into a computation.
-- This module answers the question of what happens if the composition
-- process can't finish: it creates an incomplete computation, which
-- can only return a result after something else is done to complete it.
-- 
-- Two relevant examples where these types of values can occur are:
-- 
--    - Continuation-passing style functions which, rather than returning
--      when finished, instead call another function that continues the
--      computation. The continuing function is called a /continuation/, and
--      it gets passed to the original function as an argument.
--    
--    - Interrupts, which halt the computer, and override whatever it
--      was running with their own code. These create incomplete computation
--      values in the form of /interrupt contexts/, that can be used to
--      resume the halted program once the interrupting code is finished.
-- 
-- This module mixes both of these types, giving Haskell's version of
-- interrupt operations, using a technique called /delimited continuations/.
-- The gist is that an interrupt is defined as a CPS function which, when
-- triggered, receives a continuation that encodes the interrupt context
-- of the halted program. Calling the continuation will resume the original
-- program.

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Haskell.Interrupt where

import Control.Monad.Primitive
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)


-- Note [The m ~ IO constraint]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The code here uses the weird convention of m ~ IO rather than IO
-- itself because, although the GHC primitives rely on IO, they may still
-- be pure. A distinction like this may be helpful in code that is
-- morally pure, in case the compiler isn't smart enough to know for sure.


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
