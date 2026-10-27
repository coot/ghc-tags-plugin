module Test.Timing (tests) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word64)
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Plugin.GhcTags.Timing


tests :: TestTree
tests = testGroup "Timing"
    [ testProperty "stream phases exclude upstream and downstream work" streamTimingProp
    , testProperty "lock wait excludes the action and release" lockTimingProp
    , testProperty "disabled timing preserves laziness and avoids the clock" disabledTimingProp
    ]


streamTimingProp :: [(Word16, Word16, Word16, Word16)] -> Property
streamTimingProp steps = ioProperty $ do
    clock <- newIORef 0
    report <- newIORef Nothing
    emitted <- newIORef []
    let advance amount = modifyIORef' clock (+ fromIntegral amount)
    withTimingsUsing (readIORef clock) True (writeIORef report . Just) $ \timings -> do
      let source = timedProducer timings ReadTags $
            mapM_ (\step@(readTime, _, _, _) -> Pipes.liftIO (advance readTime) >> Pipes.yield step) steps
          parsed = timedProducer timings ParseTags $ Pipes.for source $ \step@(_, parseTime, _, _) ->
            Pipes.liftIO (advance parseTime) >> Pipes.yield step
          merged = timedProducer timings MergeTags $ Pipes.for parsed $ \step@(_, _, mergeTime, _) ->
            Pipes.liftIO (advance mergeTime) >> Pipes.yield step
      Pipes.runEffect $ Pipes.for merged $ \step@(_, _, _, writeTime) -> Pipes.liftIO $
        timed timings WriteTags (advance writeTime >> modifyIORef' emitted (step :))
    actual <- readIORef report
    values <- reverse <$> readIORef emitted
    let readTime  = sum [fromIntegral r | (r, _, _, _) <- steps]
        parseTime = sum [fromIntegral p | (_, p, _, _) <- steps]
        mergeTime = sum [fromIntegral m | (_, _, m, _) <- steps]
        writeTime = sum [fromIntegral w | (_, _, _, w) <- steps]
        expected = TimingReport (readTime + parseTime + mergeTime + writeTime) $ Map.fromList
                     [(LockWait, 0), (ReadTags, readTime), (ParseTags, parseTime),
                      (GenerateTags, 0), (MergeTags, mergeTime), (WriteTags, writeTime)]
    pure $ (actual, values) === (Just expected, steps)


lockTimingProp :: Word16 -> Word16 -> Word16 -> Property
lockTimingProp waitTime actionTime releaseTime = ioProperty $ do
    clock <- newIORef 0
    report <- newIORef Nothing
    let advance amount = modifyIORef' clock (+ fromIntegral amount)
        withLock action = advance waitTime >> action <* advance releaseTime
    withTimingsUsing (readIORef clock) True (writeIORef report . Just) $ \timings ->
      timedLock timings withLock $ timed timings WriteTags $ advance actionTime
    actual <- readIORef report
    let expected = TimingReport (sum $ map fromIntegral [waitTime, actionTime, releaseTime]) $ Map.fromList
                     [(LockWait, fromIntegral waitTime), (ReadTags, 0), (ParseTags, 0),
                      (GenerateTags, 0), (MergeTags, 0), (WriteTags, fromIntegral actionTime)]
    pure $ actual === Just expected


disabledTimingProp :: [Int] -> Property
disabledTimingProp values = ioProperty $ do
    calls <- newIORef (0 :: Int)
    let forbidden = error "disabled timing used the clock or reported timings"
    result <- withTimingsUsing (forbidden :: IO Word64) False (const forbidden) $ \timings ->
      timedLock timings id $ do
        (items, _) <- timedNF timings GenerateTags $ do
          modifyIORef' calls (+ 1)
          pure (values, error "disabled timing forced the result" :: Int)
        timed timings MergeTags $ Pipes.toListM $ timedProducer timings ParseTags $ Pipes.each items
    count <- readIORef calls
    pure $ (result, count) === (values, 1)
