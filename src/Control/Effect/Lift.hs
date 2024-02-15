
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Effect.Lift where

import Control.Effect.Internal
import Control.Monad.Interrupt qualified as Ctl
import Control.Monad.Primitive
import Data.Foldable
import Data.Primitive.SmallArray
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)


class (effs0 :: [Effect]) :<< (effs :: [Effect])
    where
    targetSubIndex :: Int

instance {-# OVERLAPPING #-} effs :<< effs
    where
    targetSubIndex = 0
    {-# INLINE targetSubIndex #-}

instance (effs :<< effs0, effs1 ~ (eff : effs0)) => effs :<< effs1
    where
    targetSubIndex = targetSubIndex @effs @effs0 + 1
    {-# INLINE targetSubIndex #-}


lift :: forall effs0 effs1 a. Lift effs0 effs1 => Eff effs0 a -> Eff effs1 a
lift = Eff# . retargetVM (liftTargets @effs0 @effs1) . unEff#


class Lift (effs0 :: [Effect]) (effs1 :: [Effect])
    where
    liftTargets :: Targets# -> Targets#

instance {-# INCOHERENT #-} effs0 :<< effs1 => Lift effs0 effs1
    where
    liftTargets (Targets -> ts0) = unboxTargets ts1
        where
        idx = targetSubIndex @effs0 @effs1
        ts1 = cloneSmallArray ts0 idx (sizeofSmallArray ts0 - idx)
    {-# INLINE liftTargets #-}

instance Lift '[] effs
    where
    liftTargets _ = unboxTargets mempty
    {-# INLINE liftTargets #-}

instance (eff :< effs1, Lift effs0 effs1) => Lift (eff : effs0) effs1
    where
    liftTargets ts = unboxTargets $ runSmallArray $ do
        let null# :: Any
            null# = Any ()
        
        let ts1 = Targets (liftTargets @effs0 @effs1 ts)
        let n = sizeofSmallArray ts1
        
        ts2 <- newSmallArray (n + 1) null#
        copySmallArray ts2 1 ts1 0 n
        
        let target = indexSmallArray (Targets ts) (targetIndex @eff @effs1)
        writeSmallArray ts2 0 target
        
        pure ts2


retargetVM :: (Targets# -> Targets#) -> EVM a -> EVM a
retargetVM retarget m = EVM# $ \ts0 -> do
    let ts1 = retarget ts0
        n = sizeofSmallArray (Targets ts1)
    
    tag2 <- Ctl.newControlTag
    
    let mkSpoofTarget
            :: forall eff effs' b. (eff :< effs')
            => Target# eff
            -> eff (Eff effs') b
            -> Targets#
            -> IO (Value b)
        
        mkSpoofTarget original e evm = controlVM tag2 $ \k -> do
            -- Hit the original target.
            -- Retargetting discards any changes made to the target vector.
            a <- primitive $ \s0 ->
                case runTarget# original e ts0 s0 of
                    (# s1, _, a #) -> (# s1, a #)
            
            Ctl.delimit tag2 $ unEVM# (k a) evm
        
        spoofTarget original = Target# $ \e evm s0 ->
            case internal (mkSpoofTarget original e evm) s0 of
                (# s1, Value evm1 a #) -> (# s1, evm1, a #)
    
    let null# :: Any
        null# = Any ()
    
        ts3 = runSmallArray $ do
            ts2 <- newSmallArray n null#
    
            forM_ [0..n - 1] $ \ix -> do
                let original = indexSmallArray (Targets ts1) ix
                let new = spoofTarget (unsafeCoerce original)
                writeSmallArray ts2 ix (Any new)
            
            pure ts2
    
    Ctl.delimit tag2 $ do
        Value _ a <- unEVM# m (unboxTargets ts3)
        pure (Value ts0 a)
