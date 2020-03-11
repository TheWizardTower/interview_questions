{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib  (LRUValue, LRUCache, insertIntoCache, getFromCache) where

import           Control.Lens    (makeLenses, view, (%~), (&), (+~), (.~))
import qualified Data.Map        as M (Map, delete, foldlWithKey, insert,
                                       lookup)
import           Data.Time.Clock (UTCTime (..))
import           Prelude         (Int, Maybe (..), Ord (..), Ordering (..),
                                  return, ($))

data LRUValue v = LRUValue
    { _accessTime :: UTCTime
    , _value      :: v
    }

data LRUCache k v = LRUCache
    { _capacity      :: Int
    , _currentLength :: Int
    , _cache         :: M.Map k (LRUValue v)
    }

makeLenses ''LRUValue
makeLenses ''LRUCache

mapWithKeyFunc :: Maybe (k, LRUValue v) -> k -> LRUValue v -> Maybe (k, LRUValue v)
mapWithKeyFunc pair kr vr =
  case pair of
    Nothing -> Just (kr, vr)
    Just (kl, vl) -> let accessTimeL = view accessTime vl
                         accessTimeR = view accessTime vr
                         cmpResult = compare accessTimeL accessTimeR in
                     case cmpResult of
                           LT -> return (kr, vr)
                           EQ -> return (kr, vr)
                           GT -> return (kl, vl)

removeOldestKey :: (Ord k) => M.Map k (LRUValue v) -> M.Map k (LRUValue v)
removeOldestKey oldMap = let oldestMaybeKVPair = M.foldlWithKey mapWithKeyFunc Nothing oldMap in
                           case oldestMaybeKVPair of
                             Nothing       -> oldMap
                             Just (key, _) -> M.delete key oldMap

insertIntoCache :: (Ord k) => LRUCache k v -> k -> v -> UTCTime -> LRUCache k v
insertIntoCache inputCache key inValue insertTime =
  let currCap = view capacity inputCache
      currLen = view currentLength inputCache in
      if currCap >= currLen then
        inputCache & currentLength +~ 1
        & cache %~ M.insert key (LRUValue { _value = inValue, _accessTime = insertTime })
      else
        inputCache & cache %~ removeOldestKey
                   & cache %~ M.insert key (LRUValue { _value = inValue, _accessTime = insertTime })

updateTimestamp :: Ord k => LRUCache k v -> k -> UTCTime -> LRUCache k v
updateTimestamp inputCache key time =
  let cacheMap = view cache inputCache
      targetVal = M.lookup key cacheMap in
  case targetVal of
    Nothing -> inputCache
    Just lruVal -> inputCache & cache %~ M.insert key (lruVal & accessTime .~ time)

getFromCache :: Ord k => LRUCache k v -> k -> UTCTime -> (Maybe v, LRUCache k v)
getFromCache inputCache key readTime =
  let lruMaybe = M.lookup key (view cache inputCache) in
  case lruMaybe of
    Nothing  -> (Nothing, inputCache)
    Just val -> (Just $ view value val, updateTimestamp inputCache key readTime)
