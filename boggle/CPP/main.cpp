#include "cxxopts.hpp"
#include "trie.hpp"
#include <algorithm>
#include <iostream>
#include <set>
#include <string>
#include <vector>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::max;
using std::set;
using std::string;
using std::vector;

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

set<string> search(vector<vector<char>> boggleMatrix,
                   vector<vector<bool>> visited, Trie t, int i, int j,
                   string prefix) {

  set<string> result;
  int boardSize = ((int)boggleMatrix.size());
  if (t.searchPrefix(prefix) == false) {
    return result;
  }

  visited[i][j] = true;

  string word = prefix + boggleMatrix[i][j];
  if (t.searchWord(word)) {
    result.insert(word);
  }

  for (auto i_ = max(0, i - 1); i_ < i + 2; ++i_) {
    for (auto j_ = max(0, j - 1); j_ < j + 2; ++j_) {
      if (i_ < boardSize && j_ < boardSize && !visited[i_][j_]) {
        result += search(boggleMatrix, visited, t, i_, j_, word);
      }
    }
  }

  visited[i][j] = false;
  return result;
}

vector<string> boggleSolver(vector<vector<char>> boggleMatrix,
                            Trie dictionary) {
  set<string> result;
  auto boardSize = boggleMatrix.size();
  vector<vector<bool>> visited(boardSize, vector<bool>(boardSize, false));

  for (long unsigned int i = 0; i < boardSize; ++i) {
    for (long unsigned int j = 0; j < boardSize; ++j) {
      result += search(boggleMatrix, visited, dictionary, i, j, "");
    }
  }

  return vector<string>(result.begin(), result.end());
}

void printSet(set<int> a) {
  for (auto iter = a.begin(); iter != a.end(); ++iter) {
    cout << "Value: " << *iter << endl;
  }
}

int main() {
  vector<string> dictionary{"APP", "APPLE", "APPLY", "BUS", "BUSINESS"};
  auto t = Trie::makeTrie(dictionary);
  cout << traverseTrie(t) << endl;

  cout << "findPrefix 'APPL': " << t.searchPrefix("APPL") << endl;
  cout << "findPrefix 'BUSIN': " << t.searchPrefix("BUSIN") << endl;
  // yes, I've been watching dice, camera, action. all praise to lightfall.
  cout << "findPrefix 'BUTTTHANDER': " << t.searchPrefix("BUTTTHANDER") << endl;
  cout << "findWord 'APP': " << t.searchWord("APP") << endl;
  cout << "findWord 'APPL': " << t.searchWord("APPL") << endl;
  cout << "findWord 'BUSIN': " << t.searchWord("BUSIN") << endl;
  cout << "findWord 'BUSINESS': " << t.searchWord("BUSINESS") << endl;

  cout << traverseTrie(t) << endl;

  vector<vector<char>> boggleGrid{
      {'A', 'B', 'C'}, {'S', 'P', 'U'}, {'P', 'S', 'T'}};

  auto wordList = boggleSolver(boggleGrid, t);

  set<int> setA{1, 2, 3};
  set<int> setB{3, 4, 5, 6};

  set<int> setC = setA + setB;
  printSet(setC);

  cout << "++++" << endl;
  setA = setA + setB;

  set<int> setD{1, 2, 3};
  printSet(setA);
  cout << "++++" << endl;
  setD += set<int>{10, 11, 12};
  printSet(setD);

  cout << "Word set list print loop." << endl;
  for (auto iter = wordList.begin(); iter != wordList.end(); ++iter) {
    cout << "Word found: " << *iter << endl;
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
