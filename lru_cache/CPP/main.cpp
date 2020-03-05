#include <ctime>
#include <iostream>
#include <limits>
#include <map>
#include <optional>
#include <string>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::make_optional;
using std::map;
using std::nullopt;
using std::nullopt_t;
using std::numeric_limits;
using std::optional;
using std::ostream;
using std::string;
using std::to_string;

// This ought to be a member of the class definition, but it doesn't parse,
// because it trips on the 'class' keyword.
enum class InsertResult {
  Success,
  Success_Overwrote,
  Failure,
};

string showInsertResult(const InsertResult &ir) {
  switch (ir) {
  case InsertResult::Success:
    return "Success";
  case InsertResult::Success_Overwrote:
    return "Success_Overwrote";
  case InsertResult::Failure:
    return "Failure";
  }

  // I need to find a way to write this function without having a 'default'
  // case, while still having all the -W* options turned on. The point of sum
  // types is that something alerts if the pattern match is not exhaustive,
  // which at the moment, the above is.
  throw;
}

ostream &operator<<(ostream &os, const InsertResult &ir) {
  os << showInsertResult(ir);
  return os;
}

ostream &operator<<(ostream &os, const optional<string> &o) {
  os << o;
  return os;
}

ostream &operator<<(ostream &os, const nullopt_t) {
  os << "Nothing";
  return os;
}
enum class DeleteResult {
  Success,
  KeyNotFound,
};

class LruCache {
public:
  // Constructor. Takes the size of the 'cache' as an integer.
  LruCache(int size) {
    this->size = size;
    this->currentLength = 0;
    this->cache = {};
  }

  // Tries to insert a value. Returns what had to happen to do the insert.
  InsertResult insertKey(string key, string value) {
    if (this->size > this->currentLength) {
      this->currentLength++;
      struct LruValue tmp;
      tmp.value = value;
      tmp.lastAccessTime = time(0);
      this->cache[key] = tmp;
      return InsertResult::Success;
    }

    optional<string> overwrite_key = getLeastRecentlyUsedKey();
    if (!overwrite_key.has_value()) {
      return InsertResult::Failure;
    }
    cache.erase(overwrite_key.value());

    LruValue tmp;
    tmp.value = value;
    tmp.lastAccessTime = time(0);
    cache[key] = tmp;
    return InsertResult::Success_Overwrote;
  }

  optional<string> getKey(string key) {
    if (cache.find(key) == cache.end()) {
      return {};
    }

    cache[key].lastAccessTime = time(0);
    return cache[key].value;
  }

  DeleteResult deleteKey(string key) {
    if (cache.find(key) == cache.end()) {
      return DeleteResult::KeyNotFound;
    }

    currentLength--;
    cache.erase(key);
    return DeleteResult::Success;
  }

private:
  std::optional<string> getLeastRecentlyUsedKey() {
    optional<string> result = {};
    time_t min = numeric_limits<time_t>::max();

    for (auto iter = cache.begin(); iter != cache.end(); ++iter) {
      LruValue tmp = iter->second;
      if (min > tmp.lastAccessTime) {
        min = tmp.lastAccessTime;
        result = iter->first;
      }
    }
    return result;
  }

  struct LruValue {
    string value;
    time_t lastAccessTime;
  };
  int size;
  int currentLength;
  map<string, struct LruValue> cache;
};

template <typename T>
void validate_result(T result, T expected, string err_message) {
  if (result == expected) {
    return;
  }
  cerr << "Result not expected!" << err_message << endl;
  cerr << "Expected: " << expected << endl;
  cerr << "Result:" << result << endl;
}

int main() {
  auto my_cache = new LruCache(10);

  validate_result(my_cache->insertKey("Adam", "McCullough"),
                  InsertResult::Success, "Inserting Adam:McCullough");
  validate_result(my_cache->getKey("Adam"),
                  make_optional((string) "McCullough"), "Getting Key 'Adam'");
  validate_result(my_cache->insertKey("C++", "Rocks"), InsertResult::Success,
                  "Inserting C++:Rocks");
  validate_result(my_cache->insertKey("Key", "Value"), InsertResult::Success,
                  "Inserting Key:Value");
  for (auto i = 0; i < 7; i++) {
    string key = "Key" + to_string(i);
    string value = "Value" + to_string(i);
    string message = "Inserting  " + key + ":" + value;
    validate_result(my_cache->insertKey(key, value), InsertResult::Success,
                    message);
  }
  validate_result(my_cache->getKey("Adam"),
                  make_optional((string) "McCullough"),
                  "Getting key 'Adam' after cache is full.");
  validate_result(my_cache->getKey("C++"), make_optional((string) "Rocks"),
                  "Getting key 'C++' after cache is full.");
  validate_result(my_cache->getKey("Key"), make_optional((string) "Value"),
                  "Getting key 'Key' after cache is full.");

  for (auto i = 7; i < 10; i++) {
    string key = "Key" + to_string(i);
    string value = "Value" + to_string(i);
    string message = "Verifying that key  " + key + " Returns value " + value +
                     "After cache is filled.";
    validate_result(my_cache->insertKey(key, value), InsertResult::Success_Overwrote,
                    message);
  }

  optional<string> empty = nullopt;
  validate_result(my_cache->getKey("Adam"), empty,
                  "Verifying Adam:McCullough got LRU-Evicted.");
  validate_result(my_cache->getKey("C++"), empty,
                  "Verifying C++:Rocks got LRU-Evicted.");
  validate_result(my_cache->getKey("Key"), empty,
                  "Verifying Key:Value got LRU-Evicted.");
  return 0;
}
