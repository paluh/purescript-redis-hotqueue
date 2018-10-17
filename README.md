# purescript-redis-hotqueue

Really simple message queue based on Redis and inspired by [Python hotqueue](https://github.com/richardhenry/hotqueue).

## Queue type

Hotqueue provides this simple API:

   ``` purescript
   type Hotqueue m e a =
     { bGet ∷ m (Either e a)
     , clear ∷ m Unit
     , get ∷ m (Maybe (Either e a))
     , put ∷ a → m Unit
     , snapshot ∷ m (Array a)
     }
   ```

where `bGet` is a blocking get and `get` is non blocking.

Its `Redis` based implementation for data which are `JSON` serializable has this constructor:

   ``` purescript
   hotqueueJson
     ∷ ∀ a eff m
     . MonadAff m
     ⇒ WriteForeign a
     ⇒ ReadForeign a
     ⇒ Connection
     → Key
     → Hotqueue m MultipleErrors a
   ```

## Example

This guide is a literate Purescript file which is compiled into testing module (using [`literate-purescript`](https://github.com/Thimoteus/literate-purescript) - check `bin/docs.sh`) so it is a little verbose.

Let's start with boring stuff - imports.

``` purescript
module Test.Integration where

import Prelude

import Effect.Aff (bracket, launchAff)
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Posix.Signal (Signal(..))
import Database.Redis as Redis
import Database.Redis.Hotqueue (Hotqueue, hotqueueJson, workLoop)
import Node.ChildProcess as ChildProcess
import Test.Unit.Assert (assert)
```

Now let's define our testing environment.

``` purescript
redisPort = 43218
redisConfig = Redis.defaultConfig { port = redisPort }
inQueue = "test:input"
outQueue = "test:output"
```

Finally we are ready to define our worker. It fetches `Int`s from input queue, multiplies them by `8` and pushes the result to the output queue.

Sometimes you have to help compiler and provide type annotation for message type (`i ∷ Hotqueue _ _ Int` in this example).

``` purescript
worker = launchAff $ Redis.withConnection redisConfig $ \conn → do
  let
    (i ∷ Hotqueue _ _ Int) = hotqueueJson conn inQueue
    o = hotqueueJson conn outQueue
  void $ workLoop i \a → do
    case a of
      Right a' → o.put (a' * 8)
      Left _ → pure unit
```

Here is our test which verifies if worker done his job.

```purescript
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
```

Helpers which spawn Redis server and worker processes and cleanup them afterwards. Worker is just a `node` process run against one-liner which executes our function.

```purescript
withChild cmd args f = bracket spawn kill f
  where
  spawn = liftEffect $ ChildProcess.spawn cmd args ChildProcess.defaultSpawnOptions
  kill = void <<< liftEffect <<< ChildProcess.kill SIGABRT

withWorker f =
  withChild "node" ["-e", "require('./output/Test.Integration/index.js').worker()"] (const f)

withRedis f =
  withChild "redis-server" ["--port", show redisPort] (const f)
```

Now we are ready to start Redis server, run the worker and run testing scenario.

``` purescript
main = launchAff $ do
  withRedis $
    withWorker $
      multiplyTest
```

Let's check if we can reverse the order and start worker first and later run Redis server.
If this works let's restart Redis server and check if worker still works after that.

``` purescript
  withWorker $ do
    withRedis $
      multiplyTest

    withRedis $
      multiplyTest
```


## Testing

To run above testing scenario please call:

  ```bash
  $ pulp test --main Test.Integration
  ```
