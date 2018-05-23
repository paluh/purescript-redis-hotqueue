module Database.Redis.Hotqueue where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.ByteString (ByteString, fromUTF8, toUTF8)
import Data.Either (Either)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe)
import Data.NonEmpty (singleton)
import Data.Traversable (traverse)
import Database.Redis (Connection, REDIS, blpopIndef, lpop, lrange, ltrim, rpush)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

type Key = String

type Hotqueue m e a =
  { bGet ∷ m (Either e a)
  , clear ∷ m Unit
  , get ∷ m (Maybe (Either e a))
  , key ∷ String
  , put ∷ a → m Unit
  , snapshot ∷ m (Array (Either e a))
  }

hoist ∷ ∀ a e m n. (m ~> n) → Hotqueue m e a → Hotqueue n e a
hoist h r =
  { bGet: h r.bGet
  , clear: h r.clear
  , get: h r.get
  , key: r.key
  , put: r.put >>> h
  , snapshot: h r.snapshot
  }

workLoop
  ∷ ∀ a e m
  . MonadRec m
  ⇒ Hotqueue m e a
  → (Either e a → m Unit)
  → m Unit
workLoop queue work = forever $ do
  a ← queue.bGet
  work a

hotqueue
  ∷ ∀ a e eff m
  . MonadAff (redis ∷ REDIS | eff) m
  ⇒ Connection
  → Key
  → { ser ∷ a → String, prs ∷ String → m (Either e a) }
  → Hotqueue m e a
hotqueue conn key {ser, prs} =
  { bGet:
      (prs <<< s <<< _.value) =<< (liftAff $ blpopIndef conn (singleton (b key)))
  , clear: liftAff $ ltrim conn (b key) 1 0
  , get:
      (liftAff $ lpop conn (b key)) >>= traverse (prs <<< s)
  , key: key
  , put: void <<< liftAff <<< rpush conn (b key) <<< b <<< ser
  , snapshot: traverse (prs <<< s) =<< (liftAff $ lrange conn (b key) 0 (-1))
  }
  where
  b ∷ String → ByteString
  b = toUTF8

  s ∷ ByteString → String
  s = fromUTF8

type HotqueueJson m a = Hotqueue m MultipleErrors a

hotqueueJson
  ∷ ∀ a eff m
  . MonadAff (redis ∷ REDIS | eff) m
  ⇒ WriteForeign a
  ⇒ ReadForeign a
  ⇒ Connection
  → Key
  → Hotqueue m MultipleErrors a
hotqueueJson conn key =
  hotqueue conn key { ser: writeJSON, prs: pure <<< readJSON }
