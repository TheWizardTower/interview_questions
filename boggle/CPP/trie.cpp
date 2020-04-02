#include <list>
#include <map>
#include <string>
#include <sstream>

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


class Trie {
public:
  void addWord(string word) {
    if (word == "") {
      pathIsWord = true;
      return;
    }

    char word_character = word[0];
    string tempWord = word;
    tempWord.erase(tempWord.begin());

    Trie tempNode;
    if (children.find(word_character) != children.end()) {
      tempNode = children[word_character];
    }

    if (tempWord == "") {
      tempNode.pathIsWord = true;
    }

    if (tempWord != "") {
      tempNode.addWord(tempWord);
    }

    children[word_character] = tempNode;
    return;
  }

  bool pathIsWord = false;
  map<char, Trie> children;
};

// dictionary: N entries of 10-character words
Trie makeTrie(list<string> dictionary) {
  auto t = Trie();

  for (auto iter = dictionary.begin(); iter != dictionary.end(); ++iter) {
    t.addWord(*iter);
  }

  return t;
}

string traverseTrie(Trie t) {
  string result;
  stringstream ss;
  for (auto iter = t.children.begin(); iter != t.children.end(); ++iter) {
    ss << "Node value: '" << iter->first << "' " << iter->second.pathIsWord
       << "\n";
    ss << traverseTrie(iter->second);
  }
  return ss.str();
}
