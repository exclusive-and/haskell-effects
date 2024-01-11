
-- | Module         :   Haskell.Interrupt
--
--   Description    :   CPU (Categorical Processing Unit) Interrupt
--                      Operations
--
--   Copyright      :   (c) 2023, Simon Lovell Bart
--   License        :   BSD3 (see the file LICENSE)
--   Maintainer     :   Simon Lovell Bart
--                      <exclusiveandgate@gmail.com>
--
-- In constructive logic, Kleisli categories are a logical encoding of
-- the operational rules of computers. This module encodes the interrupt
-- operations that some computers have.
--
-- An interrupt is an operation that halts the computer so that it can
-- execute some other code. The operation also generates an interrupt
-- context, which contains information about what the computer was doing
-- before it was interrupted so that it can be resumed.
--
-- The interrupt concept presented here uses a technique called delimited
-- continuations:
--
--    - 'delimit' runs a program that can be halted by an interrupt.
--      
--    - 'control0' supplies the interrupting code, creates the interrupt
--      context, and triggers the interrupt operation.
--
-- When a program is interrupted, 'delimit' runs the code supplied by
-- 'control0'. The return value of the interrupting code becomes the final
-- result of the computation.

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
