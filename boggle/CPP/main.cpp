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

list<string> boggleTraversal(list<list<char>> boggleMatrix,
                             list<list<char>>::iterator iter_i,
                             list<char>::iterator iter_j, Trie dictionary,
                             string partialWord) {
  list<string> result;

  return result;
}

list<string> boggleSolver(list<list<char>> boggleMatrix, Trie dictionary) {
  list<string> result;
  for (auto iter_i = boggleMatrix.begin(); iter_i != boggleMatrix.end();
       ++iter_i) {
    for (auto iter_j = *iter_i->begin(); iter_j != *iter_i->end(); ++iter_j) {
      ;
    };
  }
  return result;
}

int main() {
  cout << "Hello, world!" << endl;
  list<string> dictionary{"APP", "APPLE", "APPLY", "BUS", "BUSINESS"};
  cout << "Making Trie." << endl;

  auto t = Trie::makeTrie(dictionary);

  cout << "Traversing Trie" << endl;

  cout << traverseTrie(t) << endl;

  list<list<char>> boggleGrid{
      {'A', 'B', 'C'}, {'S', 'P', 'U'}, {'P', 'S', 'T'}};

  cout << "Calling solver function." << endl;

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
