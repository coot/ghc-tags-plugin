{-# LANGUAGE NamedFieldPuns #-}

-- | Debug-only wall-clock measurements. Nested phases are charged separately.
module Plugin.GhcTags.Timing
  ( Timings
  , Phase (..)
  , TimingReport (..)
  , withTimings
  , withTimingsUsing
  , formatTimings
  , timed
  , timedNF
  , timedProducer
  , timedLock
  ) where

import Control.DeepSeq (NFData, force, rnf)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Pipes
import Text.Printf (printf)


data Phase = LockWait | ReadTags | ParseTags | GenerateTags | MergeTags | WriteTags
  deriving (Eq, Ord, Show, Enum, Bounded)

data Timings = Disabled | Enabled (IO Word64) (IORef (Map Phase Word64))

data TimingReport = TimingReport
  { totalNanos :: Word64
  , phaseNanos :: Map Phase Word64
  } deriving (Eq, Show)


withTimings :: Bool -> (TimingReport -> IO ()) -> (Timings -> IO a) -> IO a
withTimings = withTimingsUsing getMonotonicTimeNSec


-- | Injectable clock for deterministic tests. Reports completed updates only.
withTimingsUsing :: IO Word64 -> Bool -> (TimingReport -> IO ()) -> (Timings -> IO a) -> IO a
withTimingsUsing _ False _ action = action Disabled
withTimingsUsing clock True report action = do
    phases <- newIORef $ Map.fromList [(phase, 0) | phase <- [minBound .. maxBound]]
    start <- clock
    result <- action (Enabled clock phases)
    end <- clock
    elapsed <- readIORef phases
    report (TimingReport (end - start) elapsed)
    pure result


-- | Measure an action, subtracting time already charged to nested phases.
-- The action's result must be forced by the caller if evaluating it is part
-- of the measured work. No clocks or counters are used when disabled.
timed :: MonadIO m => Timings -> Phase -> m a -> m a
timed Disabled _ action = action
timed (Enabled clock phases) phase action = do
    before <- liftIO $ Map.foldl' (+) 0 <$> readIORef phases
    start <- liftIO clock
    result <- action
    end <- liftIO clock
    after <- liftIO $ Map.foldl' (+) 0 <$> readIORef phases
    liftIO $ addTime phases phase (end - start - (after - before))
    pure result


timedNF :: NFData a => Timings -> Phase -> IO a -> IO a
timedNF Disabled _ action = action
timedNF timings phase action = timed timings phase (action >>= evaluate . force)


-- | Time each pull, excluding downstream consumers and charging upstream
-- timed producers to their own phases. Retains streaming space usage.
timedProducer :: (MonadIO m, NFData a)
              => Timings -> Phase -> Pipes.Producer a m r -> Pipes.Producer a m r
timedProducer Disabled _ producer = producer
timedProducer timings phase producer = do
    next <- Pipes.lift $ timed timings phase $ do
      result <- Pipes.next producer
      case result of
        Left _ -> pure ()
        Right (value, _) -> liftIO (evaluate $ rnf value)
      pure result
    case next of
      Left result -> pure result
      Right (value, rest) -> Pipes.yield value >> timedProducer timings phase rest


-- | Measure only acquisition, not the protected action or lock release.
timedLock :: Timings
          -> (IO a -> IO a)
          -- ^ with lock combinator
          -> IO a
          -> IO a
timedLock Disabled withLock action = withLock action
timedLock (Enabled clock phases) withLock action = do
    start <- clock
    withLock $ do
      acquired <- clock
      addTime phases LockWait (acquired - start)
      action


addTime :: IORef (Map Phase Word64) -> Phase -> Word64 -> IO ()
addTime phases phase elapsed =
    modifyIORef' phases (Map.insertWith (+) phase elapsed)


formatTimings :: TimingReport -> String
formatTimings TimingReport {totalNanos, phaseNanos} =
    unwords ("timing:" : milliseconds "total" totalNanos
                      : [milliseconds (name phase) elapsed | (phase, elapsed) <- Map.toAscList phaseNanos])
  where
    milliseconds label nanos = printf "%s=%.3fms" label (fromIntegral nanos / 1000000 :: Double)
    name LockWait = "lock-wait"
    name ReadTags = "read"
    name ParseTags = "parse"
    name GenerateTags = "generate"
    name MergeTags = "merge"
    name WriteTags = "write"
