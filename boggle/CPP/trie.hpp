#pragma once

#include <list>
#include <map>
#include <string>

using std::list;
using std::map;
using std::string;

class Trie {
public:
  static Trie makeTrie(list<string> dictionary) {
    auto t = Trie();

    for (auto iter = dictionary.begin(); iter != dictionary.end(); ++iter) {
      t.addWord(*iter);
    }

    return t;
  }
  void addWord(string);
  bool pathIsWord = false;
  map<char, Trie> children;
};

Trie makeTrie(list<string>);

string traverseTrie(Trie);
