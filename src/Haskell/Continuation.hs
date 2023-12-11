
-- | Module     :   Haskell.Continuation
--
--   Copyright  :   (c) 2023, Simon Lovell Bart
--   License    :   BSD3 (see the file LICENSE)
--   Maintainer :   Simon Lovell Bart
--                  <exclusiveandgate@gmail.com>
--
-- A first-class notion of the computing machine: this module defines
-- an architecture-agnostic representation of code evaluation.

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Haskell.Continuation where

import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO (..))


-- |
data ContId a = ContId (PromptTag# a)

-- |
newContinuation :: IO (ContId a)
newContinuation = IO $ \s0 ->
    case newPromptTag# s0 of (# s1, tag #) -> (# s1, ContId tag #)

-- |
delimit :: ContId a -> IO a -> IO a
delimit (ContId tag) (IO m) = IO (prompt# tag m)

-- | The type of functions in continuation-passing style (CPS).
type CPS r a = (IO a -> IO r) -> IO r

-- |
control0 :: ContId r -> CPS r a -> IO a
control0 (ContId tag) run = IO (control0# tag continue)
    where
    continue k = case run (\(IO a) -> IO (k a)) of IO r -> r

