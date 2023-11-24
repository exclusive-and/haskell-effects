
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE UnboxedTuples  #-}

module Haskell.Continuation
    (
{- * Delimited Continuations -}
-- | #continuations#
-- 
-- Haskell's first-class delimited continuation primitives can capture
-- the continuations of running computations as values at runtime.
-- Operationally, 'delimit' and 'control0' can be understood as follows:
-- 
--    * 'delimit' does not do anything on its own; it merely indicates
--      where the sub-computation (and hence continuation) ends.
--    
--    * 'control0' halts the execution of a computation, and captures
--      whatever's left up to the enclosing 'delimit' as a continuation.
-- 
-- $runtime
    ContEv, newContinuation, delimit, CPS, control0
    ) where

import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO (..))


-- * Delimited Continuations

-- | Evidence type for identifying continuations.
data ContEv a = ContTag (PromptTag# a)

-- | Create a new continuation ID tag.
newContinuation :: IO (ContEv a)
newContinuation = IO $ \s0 ->
    case newPromptTag# s0 of (# s1, tag #) -> (# s1, ContTag tag #)

-- | See "Haskell.Effect#continuations".
delimit :: ContEv a -> IO a -> IO a
delimit (ContTag tag) (IO m) = IO (prompt# tag m)

-- | Computations in continuation-passing style.
type CPS r a = (IO a -> IO r) -> IO r

-- | See "Haskell.Effect#continuations".
control0 :: ContEv r -> CPS r a -> IO a
control0 (ContTag tag) run = IO (control0# tag continue)
    where
    continue k = case run (\(IO a) -> IO (k a)) of IO r -> r


-- $runtime
-- 
-- == Runtime Details for the Exceptionally Curious
-- 
-- At runtime, 'delimit' creates a new stack frame containing the tag,
-- before jumping into the sub-computation's code. To capture a continuation,
-- 'control0' searches its call stack for the delimiting frame. Once the
-- relevant part of the stack has been identified, it is copied to the heap.
-- 
-- After capturing the continuation, 'control0' pops all the copied frames
-- off of the stack. This is similar to what would happen if the
-- computation returned normally. Finally, to close the loop, 'control0'
-- pushes the heap pointer to the continuation onto the stack, and jumps to
-- the function in its second argument. The function argument lets the user
-- decide what to do with the captured continuation.
-- 
-- One common user function runs some extra code, then resumes right away.
-- This kind of function can be very useful when the user wants their code
-- to have access to the enclosing scope, without explicitly bringing things
-- from that scope into the computation.
-- 
-- Another common function packages the continuation in a user-defined data
-- type. This is a way to pause a computation and resume it on-demand later.
