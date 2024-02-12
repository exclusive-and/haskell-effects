
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
import Data.Primitive.SmallArray
import GHC.Exts (Any)


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

instance Lift '[] effs
    where
    liftTargets _ = unboxTargets mempty
    {-# INLINE liftTargets #-}

instance (eff :< effs1, Lift effs0 effs1) => Lift (eff : effs0) effs1
    where
    liftTargets (Targets -> ts) = unboxTargets $ runSmallArray $ do
        let n = sizeofSmallArray ts
    
        let null# :: Any
            null# = Any ()
        
        targets <- newSmallArray (n + 1) null#
        copySmallArray targets 1 ts 0 n
        
        let target = lookupTarget @effs1 @eff (unboxTargets ts)
        writeSmallArray targets 0 (Any target)
        
        pure targets


retargetVM :: (Targets# -> Targets#) -> EVM a -> EVM a
retargetVM retarget m = EVM# $ \ts -> do
    Value _ a <- unEVM# m (retarget ts)
    pure $ Value ts a
