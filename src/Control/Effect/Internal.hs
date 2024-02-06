
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Effect.Internal where

import Control.Monad (ap)
import Control.Monad.Interrupt qualified as I
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


run :: Eff '[] a -> a
run (Eff# m) = unsafeDupablePerformIO $ do
    Value _ a <- unEVM# m (unboxTargets mempty)
    pure a


send :: forall eff a effs. eff :< effs => eff (Eff effs) a -> Eff effs a
{-# INLINE send #-}
send e = Eff# (sendVM e)


-- | The type of targets visible to user-level code.
data Target eff effs r effs' a = Target {
    runTarget :: I.ControlTag (Value r) -> Targets# -> Eff effs' a
    }


compute
    ::  ( forall effs' b. eff :< effs'
            => eff (Eff effs') b
            -> Target eff effs r effs' b
        )
    -> Eff (eff : effs) r
    -> Eff effs r

{-# INLINE compute #-}
compute f e = Eff# $ computeVM f e


-- | The type of targets visible to internal code.
newtype Target# eff = Target# {
    runTarget#
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


computeVM
    :: forall eff r effs.
        ( forall effs' b. eff :< effs'
            => eff (Eff effs') b
            -> Target eff effs r effs' b
        )
    -> Eff (eff : effs) r
    -> EVM r
    
computeVM f (Eff# m0) = EVM# $ \(Targets -> ts0) -> do
    tag <- I.newControlTag
    let target evm = mkTarget# (\e -> runTarget (f e) tag evm)
    
    let n = sizeofSmallArray ts0
    
    -- This is the weird part:
    -- 
    -- The target being added needs access to the current version
    -- of the VM stack. But the VM stack needs access to the
    -- target. The workaround used here takes advantage of the fact
    -- that unsafeFreezeSmallArray doesn't copy.
    -- 
    -- The target gets a frozen reference to the array, while there
    -- is still a hot reference to the same array still floating
    -- around in here! Normally this would be very deeply unsafe!
    -- 
    -- In this case, the hot reference is only used to add the target
    -- to the array, and then gets frozen immediately after.
    -- The target never gets the chance to do anything meaningful
    -- with the array until after the hot reference is already frozen.
    
    let null# :: Any
        null# = Any ()
    
    let ts2 = runSmallArray $ do
            targets <- newSmallArray (n + 1) null#
            copySmallArray targets 1 ts0 0 n
        
            ts1 <- unsafeFreezeSmallArray targets
            writeSmallArray targets 0 (Any $ target (unboxTargets ts1))
            pure targets
    
    I.delimit tag (unEVM# m0 (unboxTargets ts2))


sendVM :: forall eff a effs. eff :< effs => eff (Eff effs) a -> EVM a
sendVM e = EVM# $ \ts -> IO $ \s0 ->
    case runTarget# (lookupTarget @effs ts) e ts s0 of
        (# s1, evm1, a #) -> (# s1, Value evm1 a #)


controlVM
    :: I.ControlTag (Value b)
    -> ((a -> EVM b) -> IO (Value b))
    -> IO (Value a)

controlVM tag f0 = I.control0 tag f1
    where
    f1 k = f0 (\a -> EVM# $ \s -> k (pure $ Value s a))

control'
    :: forall r eff effs effs' a.
       ((a -> EVM r) -> EVM r)
    -> Target eff effs r effs' a
control' f = Target $ \tag s ->
    Eff $ \_ -> controlVM tag (\k -> unEVM# (f k) s)

control0
    :: forall eff effs r effs' a.
       ((a -> Eff (eff : effs) r) -> Eff (eff : effs) r)
    -> Target eff effs r effs' a
control0 f = control' @r (coerce f)
