#include "trie.cpp"
#include <iostream>
#include <list>
#include <string>

using std::cout;
using std::endl;
using std::list;
using std::string;

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
  list<string> dictionary{"APP", "APPLE", "APPLY", "BUS", "BUSINESS"};
  auto t = makeTrie(dictionary);

  cout << traverseTrie(t) << endl;

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
