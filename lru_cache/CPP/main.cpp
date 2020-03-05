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

// deriving a 'show' instance for InsertResult.
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

// Operator definition for InsertResult, referencing the above case/switch
// function.
ostream &operator<<(ostream &os, const InsertResult &ir) {
  os << showInsertResult(ir);
  return os;
}

// Same for std::optional<string>. When values are in this type, they are their
// base type, so the expected solution of checking if the optional has a value
// does not work. We need to handle that case with a separate dispatch, defined
// immediately below this.
ostream &operator<<(ostream &os, const optional<string> &o) {
  os << o;
  return os;
}

// Handle the 'empty' case of std::optional.
ostream &operator<<(ostream &os, const nullopt_t) {
  os << "Nothing";
  return os;
}

// Specific enumeration of the possible results of the deleteKey operation in
// the LRU cache.
enum class DeleteResult {
  Success,
  KeyNotFound,
};

// Again, a show instance for the DeleteResult type.
string showDeleteResult(const DeleteResult &dr) {
  switch (dr) {
  case DeleteResult::Success:
    return "Success";
  case DeleteResult::KeyNotFound:
    return "KeyNotFound";
  }
  throw;
}

// Operator definition for DeleteResult, referencing the above case/switch
// function.
ostream &operator<<(ostream &os, const DeleteResult &ir) {
  os << showDeleteResult(ir);
  return os;
}

// Least-Recently Used Evicting Cache class. We're reasoning about 'size' in
// terms of number of values, rather than actual size of bytes in memory.
class LruCache {
public:
  // Constructor. Takes the size of the 'cache' as an integer.
  LruCache(unsigned int inputSize) {
    if (inputSize == 0) {
      throw;
    }
    size = inputSize;
    currentLength = 0;
    cache = {};
  }

  // Tries to insert a value. Returns what had to happen to do the insert. This
  // can be O(n) in the worst case, because of the search through the list for
  // the least recently-used value to evict.
  InsertResult insertKey(string key, string value) {
    if (size > currentLength) {
      this->currentLength++;
      struct LruValue tmp;
      tmp.value = value;
      tmp.lastAccessTime = time(0);
      this->cache[key] = tmp;
      return InsertResult::Success;
    }

    // If we're here, we're at capacity, so get the value to evict.
    optional<string> overwrite_key = getLeastRecentlyUsedKey();
    if (!overwrite_key.has_value()) {
      // Something deeply bizarre has happened.
      return InsertResult::Failure;
    }
    cache.erase(overwrite_key.value());

    LruValue tmp;
    tmp.value = value;
    tmp.lastAccessTime = time(0);
    cache[key] = tmp;
    return InsertResult::Success_Overwrote;
  }

  // Returns the value associated with the provided key, after updating the
  // usage time. O(1) time.
  optional<string> getKey(string key) {
    if (cache.find(key) == cache.end()) {
      return {};
    }

    cache[key].lastAccessTime = time(0);
    return cache[key].value;
  }

  // Deletes a key from the cache, if the key is found. If it is not, return a
  // failure value. If it is, reduce the size counter and return success. O(1).
  DeleteResult deleteKey(string key) {
    if (cache.find(key) == cache.end()) {
      return DeleteResult::KeyNotFound;
    }

    currentLength--;
    cache.erase(key);
    return DeleteResult::Success;
  }

private:
  // Search through the cache, find the value that was least-recently accessed.
  // O(n).
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

  // Prvate struct variable, describing the value being stored in the LRU, and
  // the time it was last accessed.
  struct LruValue {
    string value;
    time_t lastAccessTime;
  };
  int size;
  int currentLength;
  // The actual data cache.
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

  validate_result(my_cache->deleteKey("Adam"), DeleteResult::KeyNotFound,
                  "Verifying deleting a non-existent key behaves properly.");
  validate_result(my_cache->deleteKey("Key0"), DeleteResult::Success,
                  "Verifying deleting an existent key behaves properly.");

  return 0;
}
