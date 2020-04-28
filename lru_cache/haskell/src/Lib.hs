{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib  (LRUValue, LRUCache, insertIntoCache, getFromCache, makeSizedLRU) where

import           Control.Lens (makeLenses, view, (%~), (&), (+~), (.~))
import qualified Data.Map     as M (Map, delete, foldlWithKey, fromList, insert,
                                    lookup)
import           Prelude      (Eq, Int, Maybe (..), Ord (..), Ordering (..),
                               Show, return, ($), (+))

data LRUValue v = LRUValue
    { _accessTime :: Int
    , _value      :: v
    }
    deriving (Eq, Ord, Show)

data LRUCache k v = LRUCache
    { _cache         :: M.Map k (LRUValue v)
    , _capacity      :: Int
    , _currentLength :: Int
    , _maxReadTime   :: Int
    }
    deriving (Eq, Ord, Show)

makeLenses ''LRUValue
makeLenses ''LRUCache

emptyLRU :: (Ord k) => LRUCache k v
emptyLRU = LRUCache { _capacity = 0
                    , _currentLength = 0
                    , _cache = M.fromList []
                    , _maxReadTime = 0
                    }

makeSizedLRU :: (Ord k) => Int -> LRUCache k v
makeSizedLRU size = emptyLRU & capacity .~ size

mapWithKeyFunc :: Maybe (k, LRUValue v) -> k -> LRUValue v -> Maybe (k, LRUValue v)
mapWithKeyFunc pair kr vr =
  case pair of
    Nothing -> Just (kr, vr)
    Just (kl, vl) -> let accessTimeL = view accessTime vl
                         accessTimeR = view accessTime vr
                         cmpResult = compare accessTimeL accessTimeR in
                     case cmpResult of
                           LT -> return (kl, vl)
                           EQ -> return (kr, vr)
                           GT -> return (kr, vr)

removeOldestKey :: (Ord k) => M.Map k (LRUValue v) -> M.Map k (LRUValue v)
removeOldestKey oldMap = let oldestMaybeKVPair = M.foldlWithKey mapWithKeyFunc Nothing oldMap in
                           case oldestMaybeKVPair of
                             Nothing       -> oldMap
                             Just (key, _) -> M.delete key oldMap

insertIntoCache :: (Ord k) => LRUCache k v -> k -> v -> LRUCache k v
insertIntoCache inputCache key inValue =
  let currCap = view capacity inputCache
      currLen = view currentLength inputCache
      insertTime = view maxReadTime inputCache
      newVal     = LRUValue { _value = inValue, _accessTime = insertTime + 1}
      mapMaybe   = M.lookup key $ view cache inputCache in
      case mapMaybe of
        Just _ -> inputCache & maxReadTime %~ (+1)
                             & cache %~ M.delete key
                             & cache %~ M.insert key newVal
        Nothing ->
          if currLen + 1 <= currCap then
            inputCache & currentLength +~ 1
                       & maxReadTime +~ 1
                       & cache %~ M.insert key newVal
          else inputCache & maxReadTime +~ 1
                          & cache %~ removeOldestKey
                          & cache %~ M.insert key newVal

updateTimestamp :: Ord k => LRUCache k v -> k -> LRUCache k v
updateTimestamp inputCache key =
  let cacheMap = view cache inputCache
      time = view maxReadTime inputCache
      targetVal = M.lookup key cacheMap in
  case targetVal of
    Nothing -> inputCache
    Just lruVal -> inputCache & cache %~ M.insert key (lruVal & accessTime .~ time + 1)
                              & maxReadTime +~ 1

getFromCache :: Ord k => LRUCache k v -> k -> (Maybe v, LRUCache k v)
getFromCache inputCache key =
  let lruMaybe = M.lookup key (view cache inputCache) in
  case lruMaybe of
    Nothing  -> (Nothing, inputCache)
    Just val -> (Just $ view value val, updateTimestamp inputCache key)
