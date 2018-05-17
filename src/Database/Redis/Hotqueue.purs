module Database.Redis.Hotqueue where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Except (ExceptT, except)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.ByteString (ByteString, fromUTF8, toUTF8)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe)
import Data.NonEmpty (singleton)
import Data.Traversable (traverse)
import Database.Redis (Connection, REDIS, blpopIndef, lpop, lrange, ltrim, rpush)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

type Key = String

type Hotqueue m a =
  { bGet ∷ m a
  , clear ∷ m Unit
  , get ∷ m (Maybe a)
  , key ∷ String
  , put ∷ a → m Unit
  , snapshot ∷ m (Array a)
  }

hoist ∷ ∀ a m n. (m ~> n) → Hotqueue m a → Hotqueue n a
hoist h r =
  { bGet: h r.bGet
  , clear: h r.clear
  , get: h r.get
  , key: r.key
  , put: r.put >>> h
  , snapshot: h r.snapshot
  }

workLoop
  ∷ ∀ a m
  . MonadRec m
  ⇒ Hotqueue m a
  → (a → m Unit)
  → m Unit
workLoop queue work = forever $ do
  a ← queue.bGet
  work a

hotqueue
  ∷ ∀ a eff m
  . MonadAff (redis ∷ REDIS | eff) m
  ⇒ Connection
  → Key
  → { ser ∷ a → String, prs ∷ String → m a }
  → Hotqueue m a
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

type JsonMonad eff = ExceptT MultipleErrors (Aff (redis  ∷ REDIS | eff))
type HotqueueJson eff a = Hotqueue (JsonMonad eff) a

hotqueueJson
  ∷ ∀ a eff
  . WriteForeign a
  ⇒ ReadForeign a
  ⇒ Connection
  → Key
  → Hotqueue (JsonMonad eff) a
hotqueueJson conn key =
  hotqueue conn key { ser: writeJSON, prs: except <<< readJSON }
