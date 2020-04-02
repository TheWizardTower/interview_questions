#pragma once

#include <map>
#include <string>
#include <vector>

using std::map;
using std::string;
using std::vector;

class Node {
public:
  void addWord(string);
  bool searchPrefix(string);
  bool searchWord(string);
  map<char, Node> children;
  bool isWord = false;
};

class Trie {
public:
  Node rootNode;
  template <typename Iterator>
  // TODO: make Trie a generic type.
  //static Trie<decltype(*Iterator)> makeTrie(Iterator begin, Iterator end);
  static Trie makeTrie(Iterator begin, Iterator end) {
    Trie t;
    for (auto iter = begin; iter != end; ++iter) {
      t.addWord(*iter);
    }
    return t;
  }
  static Trie makeTrie(vector<string> dictionary) {
    return makeTrie(dictionary.begin(), dictionary.end());
  }

  void addWord(string);
  bool searchPrefix(string);
  bool searchWord(string);
};

string traverseTrie(Trie);
