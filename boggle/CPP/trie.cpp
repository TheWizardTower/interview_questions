#include "trie.hpp"
#include <list>
#include <map>
#include <sstream>
#include <string>

using std::list;
using std::map;
using std::string;
using std::stringstream;

/*
  APPLE
  APP
  APPLY
  BUS
  BUSINESS

  <root>
  A   B
  P   U
  P*  S*
  L    \
  | \   I
  E* Y* N
  E
  S
  S*
*/

void Trie::addWord(string word) { this->rootNode.addWord(word); }

void Node::addWord(string word) {
  string wordCopy = word;
  wordCopy.erase(wordCopy.begin());
  char currentChar = word[0];

  auto mapIter = this->children.find(currentChar);
  if (mapIter == this->children.end()) {
    Node tempNode;
    if (wordCopy == "") {
      tempNode.isWord = true;
    } else {
      tempNode.addWord(wordCopy);
    }
    this->children[currentChar] = tempNode;
    return;
  }
  mapIter->second.addWord(wordCopy);
}

bool Trie::searchPrefix(string prefix) {
  return this->rootNode.searchPrefix(prefix);
}

bool Node::searchPrefix(string prefix) {
  if (prefix == "") {
    return false;
  }
  string prefixCopy = prefix;
  char currentChar = prefix[0];
  prefixCopy.erase(prefixCopy.begin());

  auto mapIter = this->children.find(currentChar);
  if (mapIter == this->children.end()) {
    return false;
  }

  if (prefixCopy == "") {
    return true;
  }

  return mapIter->second.searchPrefix(prefixCopy);
}

bool Trie::searchWord(string word) { return this->rootNode.searchWord(word); }

bool Node::searchWord(string word) {
  if (word == "") {
    return false;
  }
  string wordCopy = word;
  char currentChar = word[0];
  wordCopy.erase(wordCopy.begin());

  auto mapIter = this->children.find(currentChar);
  if (mapIter == this->children.end()) {
    return false;
  }
  if (wordCopy == "") {
    return this->children[currentChar].isWord;
  }
  return this->children[currentChar].searchWord(wordCopy);
}

string traverseTrieNodes(Node n) {
  string result;
  stringstream ss;
  for (auto iter = n.children.begin(); iter != n.children.end(); ++iter) {
    ss << "Node value: '" << iter->first << "' " << iter->second.isWord << "\n";
    ss << traverseTrieNodes(iter->second);
  }
  return ss.str();
}

string traverseTrie(Trie t) { return traverseTrieNodes(t.rootNode); }
