#pragma once

#include <list>
#include <map>
#include <string>

using std::list;
using std::map;
using std::string;

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

  static Trie makeTrie(list<string> dictionary) {
    auto t = Trie();

    for (auto iter = dictionary.begin(); iter != dictionary.end(); ++iter) {
      t.addWord(*iter);
    }
    return t;
  }

  void addWord(string);
  bool searchPrefix(string);
  bool searchWord(string);
};

string traverseTrie(Trie);
