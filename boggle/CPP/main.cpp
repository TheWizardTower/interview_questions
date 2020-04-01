#include <iostream>
#include <list>
#include <string>

using std::cout;
using std::endl;
using std::list;
using std::string;

class Trie {
public:
  void addWord(string word) {
    if (word == "") {
      pathIsWord = true;
      return;
    }

    // check if the character exists in the list first
    char word_character = word[0];
    string tempWord = word.substr(1, std::string::npos);

    for (auto iter = children.begin(); iter != children.end(); ++iter) {
      if (iter->nodeValue == word_character) {
        iter->addWord(tempWord);
        return;
      }
    }
    Trie tempNode;
    tempNode.nodeValue = word_character;
    children.push_back(tempNode);
    tempNode.addWord(tempWord);
    return;
  }
  char getNodeValue() { return nodeValue; }
  // private:
  char nodeValue;
  bool pathIsWord = false;
  list<Trie> children;
};

// dictionary: N entries of 10-character words
Trie makeTrie(std::list<std::string> dictionary) {
  auto t = Trie();

  for (auto iter = dictionary.begin(); iter != dictionary.end(); ++iter) {
    t.addWord(*iter);
  }

  return t;
}

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




root
A P P L E
*/

int main() {
  auto words = {"Hello, ", "World!", "\n"};
  for (const string &word : words) {
    cout << word;
  }
  list<string> dictionary{"APP", "APPLE", "APPLY", "BUS", "BUSINESS"};
  auto t = makeTrie(dictionary);

  cout << "Trie root value: '" << t.nodeValue << "'" << endl;
  for (auto iter = t.children.begin(); iter != t.children.end(); ++iter) {
    cout << "  Trie child node: '" << iter->nodeValue << "'" << endl;
  }

  return 0;
}

/*

A L F

J N E

D F D

*/

/*
Given:
  - a Boggle board of size N x N, letters generated uniformly randomly
  - a dictionary of 100,000 words (uniformly random strings of 10 characters
each)
*/
