
-- | Module         :   Haskell.Continuation
--
--   Copyright      :   (c) 2023, Simon Lovell Bart
--   License        :   BSD3 (see the file LICENSE)
--   Maintainer     :   Simon Lovell Bart
--                      <exclusiveandgate@gmail.com>
--
-- A first-class notion of the computing machine: this module defines some
-- combinators that can directly control code execution.

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Haskell.Continuation where

import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO (..))


-- | 'ContTag's are how the control combinators refer to parts of the code.
data ContTag a = ContTag (PromptTag# a)

-- | Create a new 'ContTag'.
newContinuation :: IO (ContTag a)
newContinuation = IO $ \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, ContTag tag #)

-- | Delimits a section of code that 'control0' can terminate.
delimit :: ContTag a -> IO a -> IO a
delimit (ContTag tag) (IO m) = IO (prompt# tag m)

-- | The type of functions in continuation-passing style (CPS).
type CPS r a = (IO a -> IO r) -> IO r

-- | A deviant kind of function that can terminate the computation of any
-- code containing it with an artificial return value. It also creates a
-- special closure value which, when it is evaluated, causes the machine to
-- resume the terminated computation.
control0 :: ContTag r -> CPS r a -> IO a
control0 (ContTag tag) run = IO (control0# tag continue)
    where
    continue k = case run (\(IO a) -> IO (k a)) of IO r -> r
