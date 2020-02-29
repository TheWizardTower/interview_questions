#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <map>

using std::cin;
using std::cerr;
using std::cout;
using std::endl;
using std::ifstream;
using std::ostringstream;
using std::map;
using std::string;

string getInput(string filename) {
  if (filename == "") {
    ostringstream ss;
    ss << cin.rdbuf();
    string result = ss.str();
    return result;
  }
  ifstream inFile;
  inFile.open(filename);
  if (!inFile) {
    cerr << "Unable to open filename " << filename << endl;
    exit(-1);
  }
  string buf;
  string result;
  while (inFile >> buf) {
    result += buf;
  }

  return result;
}

int main() {
  string filename = "";
  string input = getInput(filename);
  map<string, int> wordMap;
  return 0;
}
