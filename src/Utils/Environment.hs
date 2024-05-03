module Utils.Environment where

import Data.Map as Map (Map, empty, fromList, insert, keys, lookup)
import qualified Data.Map.Internal.Debug as Map.Debug

-- | A mapping between labels (strings) and values
newtype Environment a = Env (Map.Map String a)

lookup :: String -> Environment a -> Maybe a
lookup label (Env env) = Map.lookup label env

insert :: String -> a -> Environment a -> Environment a
insert label arg (Env env) = Env $ Map.insert label arg env

labels :: Environment a -> [String]
labels (Env env) = Map.keys env

empty :: Environment a
empty = Env Map.empty

fromList :: [(String, a)] -> Environment a
fromList = Env . Map.fromList

instance (Show a) => Show (Environment a) where
  show (Env env) = Map.Debug.showTreeWith (\k v -> show (k, v)) True False env