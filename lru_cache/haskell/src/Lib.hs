{-# LANGUAGE NoImplicitPrelude #-}

module Lib  (LRUValue, LRUCache, insertIntoCache, getFromCache) where

import qualified Data.Map        as M (Map, delete, foldlWithKey, insert,
                                       lookup)
import           Data.Time.Clock (UTCTime (..))
import           Prelude         (Int, Maybe (..), Ord (..), Ordering (..),
                                  return, ($), (+))

data LRUValue v = LRUValue
    { accessTime :: UTCTime
    , value      :: v
    }

data LRUCache k v = LRUCache
    { capacity      :: Int
    , currentLength :: Int
    , cache         :: M.Map k (LRUValue v)
    }

mapWithKeyFunc :: Maybe (k, LRUValue v) -> k -> LRUValue v -> Maybe (k, LRUValue v)
mapWithKeyFunc pair kr vr =
  case pair of
    Nothing -> Just (kr, vr)
    Just (kl, vl) -> let accessTimeL = accessTime vl
                         accessTimeR = accessTime vr
                         cmpResult = compare accessTimeL accessTimeR in
                     case cmpResult of
                           LT -> return (kr, vr)
                           EQ -> return (kr, vr)
                           GT -> return (kl, vl)


removeOldestKey :: (Ord k) => LRUCache k v -> LRUCache k v
removeOldestKey oldCache = let map = cache oldCache
                               currCap = capacity oldCache
                               currLen = currentLength oldCache
                               oldestMaybeKVPair = M.foldlWithKey mapWithKeyFunc Nothing map in
                           case oldestMaybeKVPair of
                             Nothing -> oldCache
                             Just (key, _) -> let newMap = M.delete key map in
                               LRUCache { capacity = currCap
                                        , currentLength = currLen
                                        ,  cache = newMap
                                        }



insertIntoCache :: (Ord k) => LRUCache k v -> k -> v -> UTCTime -> LRUCache k v
insertIntoCache inputCache key inValue insertTime =
  let currCap = capacity inputCache
      currLen = currentLength inputCache
      map = cache inputCache in
    if currCap >= currLen then
      LRUCache { capacity = currCap
               , currentLength = currLen + 1
               , cache = M.insert key (LRUValue { value = inValue, accessTime = insertTime }) map
               }
    else
      let newCache = removeOldestKey inputCache
          newMap = cache newCache in
        LRUCache { capacity = currCap
                 , currentLength = currLen
                 , cache = M.insert key (LRUValue { value = inValue, accessTime = insertTime }) newMap
                 }

updateTimestamp :: Ord k => LRUCache k v -> k -> UTCTime -> LRUCache k v
updateTimestamp inputCache key time =
  let cacheMap = cache inputCache
      targetVal = M.lookup key cacheMap in
  case targetVal of
    Nothing -> inputCache
    Just lruVal -> let newMap = M.insert key LRUValue {value = value lruVal, accessTime = time} cacheMap in
      LRUCache {capacity = capacity inputCache
               , currentLength = currentLength inputCache
               , cache = newMap
               }

getFromCache :: Ord k => LRUCache k v -> k -> UTCTime -> (Maybe v, LRUCache k v)
getFromCache inputCache key readTime =
  let lruMaybe = M.lookup key (cache inputCache) in
  case lruMaybe of
    Nothing  -> (Nothing, inputCache)
    Just val -> (Just $ value val, updateTimestamp inputCache key readTime)

