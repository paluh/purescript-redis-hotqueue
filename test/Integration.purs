module Test.Integration where

import Prelude

import Control.Monad.Aff (bracket, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Posix.Signal (Signal(..))
import Database.Redis as Redis
import Database.Redis.Hotqueue (Hotqueue, hotqueueJson, workLoop)
import Node.ChildProcess as ChildProcess
import Test.Unit.Assert (assert)


redisPort = 43218
redisConfig = Redis.defaultConfig { port = redisPort }
inQueue = "test:input"
outQueue = "test:output"


worker = launchAff $ Redis.withConnection redisConfig $ \conn → do
  let
    (i ∷ Hotqueue _ _ Int) = hotqueueJson conn inQueue
    o = hotqueueJson conn outQueue
  void $ workLoop i \a → do
    case a of
      Right a → o.put (a * 8)
      Left _ → pure unit


multiplyTest =
  Redis.withConnection redisConfig \conn → do
    let
      i = hotqueueJson conn inQueue
      (o ∷ Hotqueue _ _ Int) = hotqueueJson conn outQueue
      args = [1,2,3,4,5,6]

    for_ args \n → do
      void $ i.put n

    for_ args \n → do
      x ← o.bGet
      assert "Result has been correctly calculated" (x == Right (n * 8))


withChild cmd args f = bracket spawn kill f
  where
  spawn = liftEff $ ChildProcess.spawn cmd args ChildProcess.defaultSpawnOptions
  kill = void <<< liftEff <<< ChildProcess.kill SIGABRT

withWorker f =
  withChild "node" ["-e", "require('./output/Test.Integration/index.js').worker()"] (const f)

withRedis f =
  withChild "redis-server" ["--port", show redisPort] (const f)


main = launchAff $ do
  withRedis $
    withWorker $
      multiplyTest


  withWorker $ do
    withRedis $
      multiplyTest

    withRedis $
      multiplyTest

