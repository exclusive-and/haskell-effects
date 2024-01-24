
-- | Module         :   Haskell.Interrupt
--
--   Description    :   Categorical Semantics of Interrupt Operations
--
--   Copyright      :   (c) 2023-2024, Simon Lovell Bart
--   License        :   BSD3 (see the file LICENSE)
--   Maintainer     :   Simon Lovell Bart
--                      <exclusiveandgate@gmail.com>
--
-- In constructive logic, computers are characterized by Kleisli
-- categories. This module concerns categories with values that represent
-- incomplete computations, which can only return a result after
-- something else is done to complete them. Two relevant examples where
-- these types of values can occur are:
-- 
--    - Continuation-passing style functions, which as the name suggests,
--      are completed by passing a continuation representing the remainder
--      of the computation.
--    
--    - Interrupts, which halt a computer and override whatever the
--      computer was running with their own code. These create incomplete
--      computation values in the form of /interrupt contexts/, that can
--      be used to resume the halted program once the interrupting code is
--      finished.
-- 
-- This module mixes both of these types, giving Haskell's version of
-- interrupt operations, using a technique called /delimited continuations/.
-- The gist is that interrupts are defined as CPS functions which, when
-- triggered, receive a continuation that encodes the interrupt context
-- of the halted program. That way, the interrupt code can decide whether
-- to resume the program (by calling the continuation).
-- Alternatively, it can return the context's value, so that subsequent
-- code can decide what to do with it (as in threaded process scheduling).

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Haskell.Interrupt where

import Control.Monad.Primitive
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)


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
