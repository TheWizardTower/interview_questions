#include "cxxopts.hpp"
#include "trie.hpp"
#include <iostream>
#include <list>
#include <string>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::ifstream;
using std::list;
using std::string;

template <typename T> set<T> operator+(const set<T> &a, const set<T> &b) {
  set<T> s = set<T>(a.begin(), a.end());
  for (const auto &element : b) {
    s.insert(element);
  }
  return s;
}

template <typename T> set<T> operator+=(set<T> &lVal, const set<T> &&rVal) {
  for (const auto &element : rVal) {
    lVal.insert(element);
  }
  return lVal;
}

  return result;
}

list<string> boggleSolver(list<list<char>> boggleMatrix, Trie dictionary) {
  list<string> result;
  auto boardSize = boggleMatrix.size();
  list<list<bool>> visited(boardSize, list<bool>(boardSize, false));

  for (auto iter_i = boggleMatrix.begin(); iter_i != boggleMatrix.end();
       ++iter_i) {
    for (auto iter_j = *iter_i->begin(); iter_j != *iter_i->end(); ++iter_j) {
      ;
    };
  }
  return result;
}

int main() {
  list<string> dictionary{"APP", "APPLE", "APPLY", "BUS", "BUSINESS"};
  auto t = Trie::makeTrie(dictionary);
  cout << traverseTrie(t) << endl;

  cout << "findPrefix 'APPL': " << t.searchPrefix("APPL") << endl;
  cout << "findPrefix 'BUSIN': " << t.searchPrefix("BUSIN") << endl;
  cout << "findPrefix 'BUTTTHANDER': " << t.searchPrefix("BUTTTHANDER")
       << endl; // yes, I've been watching dice, camera, action.
  cout << "findWord 'APP': " << t.searchWord("APP") << endl;
  cout << "findWord 'APPL': " << t.searchWord("APPL") << endl;
  cout << "findWord 'BUSIN': " << t.searchWord("BUSIN") << endl;
  cout << "findWord 'BUSINESS': " << t.searchWord("BUSINESS") << endl;

  cout << traverseTrie(t) << endl;

  list<list<char>> boggleGrid{
      {'A', 'B', 'C'}, {'S', 'P', 'U'}, {'P', 'S', 'T'}};

  auto wordList = boggleSolver(boggleGrid, t);

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
