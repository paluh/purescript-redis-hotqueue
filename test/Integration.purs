module Test.Integration where

import Prelude

import Control.Monad.Aff (bracket, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Posix.Signal (Signal(..))
import Database.Redis as Redis
import Database.Redis.Hotqueue (Hotqueue, HotqueueJson, hotqueueJson, workLoop)
import Node.ChildProcess as ChildProcess
import Test.Unit.Assert (assert)

withChild spawn f = bracket spawn kill f
  where
  kill = void <<< liftEff <<< ChildProcess.kill SIGABRT

withSpawn cmd args opts = withChild spawn
  where
  spawn = liftEff $ ChildProcess.spawn cmd args opts

withFork cmd args = withChild spawn
  where
  spawn = liftEff $ ChildProcess.fork cmd args


redisPort = 43218
redisConfig = Redis.defaultConfig { port = redisPort }
inQueue = "test:input"
outQueue = "test:output"


worker = launchAff $ Redis.withConnection redisConfig $ \conn → do
  let
    (i ∷ Hotqueue _ Int) = hotqueueJson conn inQueue
    o = hotqueueJson conn outQueue
  void $ runExceptT $ workLoop i \a → do
    o.put (a * 8)


main = launchAff $ do
  withSpawn "redis-server" ["--port", show redisPort] ChildProcess.defaultSpawnOptions $ const $
    withFork "./test/worker.js" [] $ const $
      Redis.withConnection redisConfig \conn → do
        let
          i = hotqueueJson conn inQueue
          (o ∷ HotqueueJson _ Int) = hotqueueJson conn outQueue
          args = [1,2,3,4,5,6]
        for_ args \n → do
          void $ runExceptT $ i.put n
        for_ args \n → do
          x ← runExceptT $ o.bGet
          assert "Result has been correctly calculated" (x == Right (n * 8))
