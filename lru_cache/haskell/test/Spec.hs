import           Data.Foldable (foldl')
import           Data.Map      as M (fromList)
import           Lib           (LRUCache (..), LRUValue (..), insertIntoCache,
                                makeSizedLRU)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  let myCache = makeSizedLRU 5
      emptyCache = LRUCache { _cache = M.fromList [], _capacity = 10, _currentLength = 0, _maxReadTime = 0 } :: LRUCache String String
      mapVals = [("Adam", "McCullough")
                , ("Daggerfall", "Fun Dungeon Torture")
                , ("Hollow Knight", "Metroidvanya")
                , ("ARMA 3", "Fun Milsim Torture")
                , ("EUIV", "History Nerd Candy")
                , ("Adam", "Overwrite-test")
                , ("Neverwinter Nights", "NERRRRRRDS")
                ]
      myCache' = foldl' (\cache (key, val) -> insertIntoCache cache key val) myCache mapVals
      myCache'' = insertIntoCache myCache' "key" "value"
      myCache'3 = insertIntoCache myCache'' "key1" "value1"
      myCache'4 = insertIntoCache myCache'3 "Adam" "Did I get overwritten?"
  describe "Build a cache." $
    it "Should build an empty cache correctly" $
      emptyCache `shouldBe` LRUCache { _cache = M.fromList [], _capacity = 10, _currentLength = 0, _maxReadTime = 0}
  describe "Populate a cache" $
    it "Should populate a cache correctly." $
      myCache' `shouldBe` (LRUCache {
        _cache = M.fromList [("ARMA 3", LRUValue {_accessTime = 4, _value = "Fun Milsim Torture"})
                          ,("Adam", LRUValue {_accessTime = 6, _value = "Overwrite-test"})
                          ,("EUIV", LRUValue {_accessTime = 5, _value = "History Nerd Candy"})
                          ,("Hollow Knight", LRUValue {_accessTime = 3, _value = "Metroidvanya"})
                          ,("Neverwinter Nights", LRUValue {_accessTime = 7, _value = "NERRRRRRDS"})
                          ]
      ,_capacity = 5
      ,_currentLength = 5
      ,_maxReadTime = 7
      })
  describe "insertIntoCache" $ do
    it "Should evict the LRU correctly." $
      myCache'' `shouldBe` (LRUCache {
        _cache = M.fromList [("ARMA 3", LRUValue {_accessTime = 4, _value = "Fun Milsim Torture"})
                          ,("Adam", LRUValue {_accessTime = 6, _value = "Overwrite-test"})
                          ,("EUIV", LRUValue {_accessTime = 5, _value = "History Nerd Candy"})
                          ,("Neverwinter Nights", LRUValue {_accessTime = 7, _value = "NERRRRRRDS"})
                          ,("key", LRUValue {_accessTime = 8, _value = "value"})
                          ]
        ,_capacity = 5
        ,_currentLength = 5
        ,_maxReadTime = 8
        })
    it "Should evict the LRU correctly again!" $
      myCache'3 `shouldBe` (LRUCache {
        _cache = M.fromList [("Adam", LRUValue {_accessTime = 6, _value = "Overwrite-test"})
                          ,("EUIV", LRUValue {_accessTime = 5, _value = "History Nerd Candy"})
                          ,("Neverwinter Nights", LRUValue {_accessTime = 7, _value = "NERRRRRRDS"})
                          ,("key", LRUValue {_accessTime = 8, _value = "value"})
                          ,("key1", LRUValue {_accessTime = 9, _value = "value1"})
                          ]
        ,_capacity = 5
        ,_currentLength = 5
        ,_maxReadTime = 9
        })
    it "Should overwrite values without evicting, even when full." $
      myCache'4 `shouldBe` (LRUCache {
        _cache = M.fromList [("Adam", LRUValue {_accessTime = 10, _value = "Did I get overwritten?"})
                          ,("EUIV", LRUValue {_accessTime = 5, _value = "History Nerd Candy"})
                          ,("Neverwinter Nights", LRUValue {_accessTime = 7, _value = "NERRRRRRDS"})
                          ,("key", LRUValue {_accessTime = 8, _value = "value"})
                          ,("key1", LRUValue {_accessTime = 9, _value = "value1"})
                          ]
        ,_capacity = 5
        ,_currentLength = 5
        ,_maxReadTime = 10
        })
