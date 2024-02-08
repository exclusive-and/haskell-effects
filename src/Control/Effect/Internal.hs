
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Effect.Internal where

import Control.Monad (ap)
import Control.Monad.Interrupt qualified as Ctl
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Coerce
import Data.Kind (Type)
import Data.Primitive.SmallArray
import GHC.Exts (Any, Int (..), SmallArray#, indexSmallArray#, State#)
import GHC.IO (IO (..), unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)


-- [Attribution]
-- ~~~~~~~~~~~~~
-- Like many of its contemporaries, this entire module is based on
-- lexi-lambda's wonderful eff package.
-- 
-- The package can be found at: https://github.com/lexi-lambda/eff/


type Effect = (Type -> Type) -> Type -> Type

newtype Eff (effs :: [Effect]) a = Eff# { unEff# :: EVM a }
    deriving (Functor, Applicative, Monad)

class (eff :: Effect) :< (effs :: [Effect])
    where
    targetIndex :: Int

instance {-# OVERLAPPING #-} eff :< (eff : effs)
    where
    targetIndex = 0
    {-# INLINE targetIndex #-}

instance eff :< effs => eff :< (eff' : effs)
    where
    targetIndex = targetIndex @eff @effs + 1
    {-# INLINE targetIndex #-}


-- Note [Some Effects Are Morally Pure]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The code in this module is a prime example of /moral purity/ as described
-- in Note [Weird monad constraints] in Control.Monad.Interrupt.
-- 
-- Everything in here is backed one way or another by IO out of consideration
-- for performance. But many effectful operations have equivalent pure
-- implementations, using monad transformer stacks or free monads, that don't
-- necessitate IO at all.
-- 
-- By virtue of their moral purity, these sorts of effects can be safely
-- run to completion in-place using unsafeDupablePerformIO.
-- 
-- For effects that really are impure and do necessitate IO, the alternative
-- runIO exposes the underlying IO computation of the effect.

run :: Eff '[] a -> a
-- See Note [Some Effects are Morally Pure]
run (Eff# m) = unsafeDupablePerformIO $ do
    Value _ a <- unEVM# m (unboxTargets mempty)
    pure a

compute
    :: (i -> Eff effs r)
    ->  ( forall effs' a. eff :< effs'
            => eff (Eff effs') a
            -> Target eff effs i r effs' a
        )
    -> Eff (eff : effs) i
    -> Eff effs r

{-# INLINE compute #-}
compute onReturn f e = Eff# $ computeVM (coerce onReturn) f (unEff# e)

send :: forall eff a effs. eff :< effs => eff (Eff effs) a -> Eff effs a
{-# INLINE send #-}
send e = Eff# (sendVM e)


data IOE :: Effect where
    LiftIO :: IO a -> IOE m a

runIO :: Eff '[IOE] a -> IO a
runIO (Eff# m0) = do
    let computeIO = computeVM pure $ \case
            LiftIO m1 -> locally (Eff $ \s -> Value s <$> liftIO m1)
    Value _ a <- unEVM# (computeIO m0) (unboxTargets mempty)
    pure a

instance IOE :< effs => MonadIO (Eff effs)
    where
    liftIO = send . LiftIO
    {-# INLINE liftIO #-}


-- Note [Two-tiered Delimiters]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some of the effectful delimited control operations disagree on exactly
-- what they'd like to have in their continuations.
-- 
-- There are two possibilities:
-- 
--   1. The continuation contains the main exit path of the computation;
--      therefore its type will be a -> Eff effs r, where r is the final
--      return type of the computation.
--   
--   2. The continuation DOES NOT contain the exit path; thus its type
--      is a -> Eff effs i, where i is an intermediate value type that will
--      require some post-processing to convert to r.
-- 
-- In the first case, someone calling the continuation can expect it to
-- finish up all the work for them, and directly return or use its result.
-- The second case will necessitate that the caller does the aforementioned
-- post-processing, since the final result can still only be r.
-- 
-- Actually accomplishing the described behaviour requires some trickery:
-- note that Ctl.control0, which is ultimately what backs delimited control,
-- can only exit with the same type that their continuations return, i.e.
-- 
--      Ctl.control0 tag (\(k :: a -> IO r) -> ... :: IO r)
-- 
-- works fine for the first behaviour. But the second behaviour is impossible
-- because
-- 
--      Ctl.control0 tag (\(k :: a -> IO i) -> ... :: IO r)
-- 
-- requires that i ~ r, which doesn't hold in general.
-- 
-- The approach used to remedy this unification desperation involves two
-- distinct control tags, and inner and outer delimiters like so:
-- 
--      tag0 <- Ctl.newControlTag @i    <-- tag for /inner/ delimiter
--      tag1 <- Ctl.newControlTag @r    <-- tag for /outer/ delimiter
-- 
--      Ctl.delimit tag1 $ do           <-- outer delimiter
--          a <- Ctl.delimit tag0 m     <-- inner delimiter
--          onReturn a                  <-- main exit path of the computation
-- 
-- Implementing the first behaviour is still simple, as control returns to
-- the outer delimiter:
-- 
--      Ctl.control0 tag1 (\(k :: a -> IO r) -> ... :: IO r)
-- 
-- The second behaviour can be implemented cleverly as:
-- 
--      Ctl.control0 tag0 $                 <-- returns to inner delimiter
--          \(k :: a -> IO i) ->
--              (
--                  Ctl.control0 tag1 $     <-- returns to outer from inner
--                      \(_ :: i -> IO r) -> ... :: IO r
--              ) :: IO i
-- 
-- In other words, by returning to the outer delimiter from the inner one,
-- the second Ctl.control0 totally bypasses the main exit path, while
-- maintaining the proper types throughout. The continuation of the second
-- Ctl.control0 is totally ignored, since it only contains the exit path
-- which was never intended to be resumed.


-- Note [External vs Internal Targets]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- There are two types of targets, external (visible to users) and internal
-- (visible to the effect VM).
-- 
-- The two types are distinguished by how much information is needed to
-- call them:
-- 
-- [External Targets]
--      Require the effect being targetted, the two delimited control tags,
--      a capture helper, and the target vector constructed in computeVM when
--      first installing.
-- 
-- [Internal Targets]
--      Require only the effect being targetted and the target vector at the
--      sendVM call site.
-- 
-- All of the parameters of the external target are created and provided in
-- computeVM, where the internal target is constructed.
-- 
-- The internal target has access to all of the external target's parameters
-- through its closure. It gets stored in the target vector, until it is
-- looked up and called by sendVM, which provides the effect and the call site
-- target vector.


-- | The type of targets visible to user-level code.
data Target eff effs i r effs' a = Target {
    mkTarget
        -- See Note [External vs Internal Targets]
        -- and Note [Two-tiered Delimiters]
        :: Ctl.ControlTag (Value i)
        -> Ctl.ControlTag (Value r)
        -> (Capture i r -> IO (Value r))
        -> Targets#
        -> Eff effs' a
    }

-- | The type of targets visible to internal code.
newtype Target# eff = Target# {
    runTarget#
        -- See Note [External vs Internal Targets]
        :: forall effs a. eff :< effs
        => eff (Eff effs) a
        -> Targets#
        -> State# RealWorld
        -> (# State# RealWorld, Targets#, a #)
    }

-- | Create an internal-level target from a user-level one.
mkTarget#
    :: (forall effs a. eff :< effs => eff (Eff effs) a -> Eff effs a)
    -> Target# eff

-- See Note [External vs Internal Targets]
{-# INLINE mkTarget# #-}
mkTarget# f = Target# $ \e evm0 s0 ->
    case internal (unEVM# (unEff# $ f e) evm0) s0 of
        (# s1, Value evm1 a #) -> (# s1, evm1, a #)


newtype Targets# = Targets# (SmallArray# Any)

pattern Targets :: Targets# -> SmallArray Any
{-# COMPLETE Targets #-}
pattern Targets {unboxTargets} <- SmallArray (Targets# -> unboxTargets)
    where
    Targets (Targets# ts) = SmallArray ts

pattern Any :: forall a. a -> Any
{-# COMPLETE Any #-}
pattern Any a <- (unsafeCoerce -> a)
    where
    Any a = unsafeCoerce a

lookupTarget :: forall effs eff. eff :< effs => Targets# -> Target# eff
lookupTarget (Targets# ts) =
    case targetIndex @eff @effs of
        I# n -> case indexSmallArray# ts n of
            (# Any t #) -> t


-- Note [Effect Virtual Machine]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--

-- |
newtype EVM a = EVM# { unEVM# :: Targets# -> IO (Value a) }

data Value a = Value Targets# ~a

pattern Eff :: (Targets# -> IO (Value a)) -> Eff effs a
{-# COMPLETE Eff #-}
pattern Eff {unEff} = Eff# (EVM# unEff)


sendVM :: forall eff a effs. eff :< effs => eff (Eff effs) a -> EVM a
sendVM e = EVM# $ \ts -> IO $ \s0 ->
    case runTarget# (lookupTarget @effs ts) e ts s0 of
        (# s1, evm1, a #) -> (# s1, Value evm1 a #)

controlVM
    :: Ctl.ControlTag (Value b)
    -> ((a -> EVM b) -> IO (Value b))
    -> IO (Value a)

{-# INLINE controlVM #-}
controlVM tag f0 = Ctl.control0 tag f1
    where
    f1 k = f0 (\a -> EVM# $ \s -> k (pure $ Value s a))

data Capture i r    -- See Note [Two-tiered Delimiters]
    where
    Include :: ((a -> EVM r) -> EVM r)
            -> (a -> EVM r)
            -> Targets#
            -> Capture i r

    Exclude :: ((a -> EVM i) -> EVM r)
            -> (a -> EVM i)
            -> Targets#
            -> Capture i r


instance Functor EVM
    where
    fmap f m = m >>= pure . f
    {-# INLINE fmap #-}

instance Applicative EVM
    where
    pure a = EVM# $ \s -> pure (Value s a)
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

instance Monad EVM
    where
    EVM# m >>= f = EVM# $ \s0 -> do
        Value s1 a <- m s0
        unEVM# (f a) s1
    {-# INLINE (>>=) #-}


-- Note [Installing Targets]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- A new target vector gets created and copied from the old one each time
-- a target gets installed.
-- 
-- Paradoxically, the target being installed will need access to the new
-- target vector that it's installed in.
-- 
-- Fortunately the targets (shouldn't) ever actually /do/ anything with their
-- reference to the target vector until much later when they're invoked by
-- sendVM. With that in mind, the gameplan is:
-- 
--   1. Create a new SmallArray to back the new target vector.
--   
--   2. Copy the old targets over to the new array.
--   
--   3. Use unsafeFreezeSmallArray to obtain a frozen reference to the new
--      vector /without copying/ so that it points to the same place as the
--      still-existing and mutable hot reference.
-- 
--   4. Pass the frozen reference to the target vector to the target.
--   
--   5. Finally, add the target to the vector using the hot reference.
-- 
-- Once all these steps are done, runSmallArray returns another frozen
-- reference to the new target vector to the downstream code, consuming the
-- now-unnecessary hot reference in the process.

computeVM
    :: forall i r eff effs.
       (i -> EVM r)
    ->  ( forall effs' a. eff :< effs'
            => eff (Eff effs') a
            -> Target eff effs i r effs' a
        )
    -> EVM i
    -> EVM r
    
computeVM onReturn f m0 = EVM# $ \(Targets -> ts0) -> do
    -- See Note [Two-tiered Delimiters]
    tag0 <- Ctl.newControlTag
    tag1 <- Ctl.newControlTag
    
    let onCapture :: Capture i r -> IO (Value r)
        onCapture capture = case capture of
            Include g k ts -> do
                -- Only need to reinstall the outer delimiter as the inner
                -- one is captured in the continuation,
                -- as described in Note [Two-tiered Delimiters].
                let k' x = EVM# $ \ts' -> Ctl.delimit tag1 (unEVM# (k x) ts')
                unEVM# (g k') ts
            
            Exclude g k ts -> unEVM# (g k) ts
            
    -- See Note [External vs Internal Targets]
    let target evm = mkTarget# (\e -> mkTarget (f e) tag0 tag1 onCapture evm)
    
    let n = sizeofSmallArray ts0
    
    let null# :: Any
        null# = Any ()
    
    -- See Note [Installing Targets]
    let ts2 = runSmallArray $ do
            targets <- newSmallArray (n + 1) null#
            copySmallArray targets 1 ts0 0 n
            ts1 <- unsafeFreezeSmallArray targets
            writeSmallArray targets 0 (Any $ target (unboxTargets ts1))
            pure targets
    
    -- See Note [Two-tiered Delimiters]
    Ctl.delimit tag1 $ do
        Value ts3 a <- Ctl.delimit tag0 (unEVM# m0 (unboxTargets ts2))
        unEVM# (onReturn a) ts3


control
    :: forall i r eff effs effs' a.
       ((a -> Eff effs r) -> Eff effs r)
    -> Target eff effs i r effs' a
control f =
    Target $ \_tag0 tag1 onCapture s ->
        Eff $ \_ ->
            -- See Note [Two-tiered Delimiters]
            controlVM tag1 $ \k -> onCapture $! Include (coerce f) k s


control0
    :: forall i r eff effs effs' a.
       ((a -> Eff (eff : effs) i) -> Eff effs r)
    -> Target eff effs i r effs' a
control0 f =
    Target $ \tag0 tag1 onCapture s ->
        Eff $ \_ ->
            -- See Note [Two-tiered Delimiters]
            controlVM tag0 $ \k ->
                controlVM tag1 $ \_ -> onCapture $! Exclude (coerce f) k s


locally :: Eff effs' a -> Target eff effs i r effs' a
locally m = Target $ \_tag0 _tag1 _onCapture _s -> m
