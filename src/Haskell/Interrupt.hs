
-- | Module         :   Haskell.Interrupt
--
--   Description    :   CPU (Categorical Processing Unit) Interrupt
--                      Operations
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
-- 
-- 

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Haskell.Interrupt where

import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO (..))


-- | 
data ControlTag a = ControlTag (PromptTag# a)

-- | 
newControlTag :: IO (ControlTag a)
newControlTag = IO $ \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, ControlTag tag #)


-- | 
delimit :: ControlTag a -> IO a -> IO a
delimit (ControlTag tag) (IO m) = IO (prompt# tag m)


-- | The type of functions in continuation-passing style (CPS).
type CPS r a = (IO a -> IO r) -> IO r

-- | A deviant kind of function that can terminate the computation of any
-- code containing it with an artificial return value. It also creates a
-- special closure value which, when it is evaluated, causes the machine to
-- resume the terminated computation.
control0 :: ControlTag r -> CPS r a -> IO a
control0 (ControlTag tag) run = IO (control0# tag continue)
    where
    continue k = case run (\(IO a) -> IO (k a)) of IO r -> r
