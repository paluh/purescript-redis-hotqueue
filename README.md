# purescript-redis-hotqueue

Really simple message queue based on Redis and inspired by [Python hotqueue](https://github.com/richardhenry/hotqueue).

## Queue type

Hotqueue provides this simple API:

   ``` purescript
   type Hotqueue m a =
     { bGet ∷ m a
     , clear ∷ m Unit
     , get ∷ m (Maybe a)
     , key ∷ String
     , put ∷ a → m Unit
     , snapshot ∷ m (Array a)
     }
   ```

Its `Redis` based implementation for data which are `JSON` serializable has this constructor:

   ``` purescript
   hotqueueJson
     ∷ ∀ a eff
     . WriteForeign a
     ⇒ ReadForeign a
     ⇒ Redis.Connection
     → Key
     → Hotqueue
         (ExceptT MultipleErrors (Aff (redis  ∷ REDIS | eff)))
         a
   ```

## Example

Caution! This guide is literate Purescript which is turned into testing module so it is complete and a little verbose.

So let's start with boring stuff: imports + test helpers.

``` purescript
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
```

Now let's define our testing environment.

``` purescript
redisPort = 43218
redisConfig = Redis.defaultConfig { port = redisPort }
inQueue = "test:input"
outQueue = "test:output"
```

Finally we are ready to define our worker. It fetches `Int`s from input queue, multiplies them by `8` and pushes the result to the output queue.

Sometimes you have to help and provide type annotation for message type (`i ∷ Hotqueue _ Int` in this example)...


``` purescript
worker = launchAff $ Redis.withConnection redisConfig $ \conn → do
  let
    (i ∷ Hotqueue _ Int) = hotqueueJson conn inQueue
    o = hotqueueJson conn outQueue
  void $ runExceptT $ workLoop i \a → do
    o.put (a * 8)
```

Now we are ready to start redis server, run worker...

``` purescript
main = launchAff $ do
  withSpawn "redis-server" ["--port", show redisPort] ChildProcess.defaultSpawnOptions $ const $
    withFork "./test/worker.js" [] $ const $

```

... push some data and check the result.

```
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

