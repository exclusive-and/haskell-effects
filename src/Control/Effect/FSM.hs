
module Control.Effect.FSM where

import Control.Monad.Interrupt
import Control.Effect
import Data.IORef
import Data.Primitive.Array
import GHC.Exts (RealWorld)


type FSM i o = Eff (Latching i o)

data Latching i o a where
    Tick :: o -> Latching i o i

tick :: o -> FSM i o i
tick x = send (Tick x)


data FSMStatus i o a
    = Done a
    | Ticking o (IO i -> IO (FSMStatus i o a))

runFSM :: FSM i o a -> IO (FSMStatus i o a)
runFSM (Eff m) = do
    tag <- newControlTag
    delimit tag (fmap Done . m $ fsmTarget tag)

fsmTarget
    :: ControlTag (FSMStatus i o a)
    -> Target (Latching i o) (FSMStatus i o a)

fsmTarget tag = Target tag $ \(Tick x) k -> pure (Ticking x (delimit tag . k))


data Module i o = Module (FSM i o ()) (IORef (IO i -> IO (FSMStatus i o ())))

mkModule :: FSM i o () -> IO (Module i o)
mkModule fsm = do
    r <- runFSM fsm
    case r of
        Done _ -> error "FSM exited"
        Ticking _ k -> do
            ref <- newIORef k
            pure $ Module fsm ref

tickModule :: Module i o -> i -> IO o
tickModule (Module _fsm ref) i = do
    k <- readIORef ref
    r <- k (pure i)
    case r of
        Done _ -> error "FSM exited"
        Ticking x k' -> do
            writeIORef ref k'
            pure x


readReg :: IORef a -> FSM i o a
readReg reg = Eff $ \_ -> readIORef reg

writeReg :: IORef a -> a -> FSM i o ()
writeReg reg x = Eff $ \_ -> writeIORef reg x


type MArray a = MutableArray RealWorld a

readRam :: MutableArray RealWorld a -> Int -> FSM i o a
readRam ram i = Eff $ \_ -> readArray ram i

writeRam :: MutableArray RealWorld a -> Int -> a -> FSM i o ()
writeRam ram i x = Eff $ \_ -> writeArray ram i x



