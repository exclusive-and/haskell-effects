
{-| Module          :   Control.Effect

    Copyright       :   (c) 2023-2024, Simon Lovell Bart
    License         :   BSD3 (see the file LICENSE)

    Maintainer      :   Simon Lovell Bart <exclusiveandgate@gmail.com>
    Stability       :   experimental
    Portability     :   non-portable (GHC Extensions)

    Algebraic effects are an alternative to side-effects, where effects
    are explained by logic types.

    
-}

module Control.Effect (
    Effect, Eff, (:<), run,
    compute, send,
    Target,
    IOE, runIO,
    control, control0, locally
    ) where

import Control.Effect.Internal
