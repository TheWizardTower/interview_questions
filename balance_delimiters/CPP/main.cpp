#include <iostream>
#include <map>
#include <stack>
#include <vector>

using std::cout;
using std::endl;
using std::map;
using std::stack;
using std::string;
using std::vector;

bool isBalanced(string input) {
  stack<char> delimiters;
  map<char, char> openDelim;
  openDelim[')'] = '(';
  openDelim[']'] = '[';
  openDelim['}'] = '{';

  for (auto const &character : input) {
    switch (character) {
    case '\'':
      if (delimiters.size() != 0 && delimiters.top() == '\'') {
        delimiters.pop();
        continue;
      }
      if (delimiters.size() != 0 && delimiters.top() != '\'') {
        delimiters.push(character);
        continue;
      }
    case '"':
      if (delimiters.size() != 0 && delimiters.top() == '"') {
        delimiters.pop();
        continue;
      }
      if (delimiters.size() != 0 && delimiters.top() != '"') {
        delimiters.push(character);
        continue;
      }
    case '(':
    case '[':
    case '{':
      if (delimiters.size() != 0 &&
          (delimiters.top() == '\'' || delimiters.top() == '"')) {
        continue;
      }
      delimiters.push(character);
      break;
    case ')':
    case ']':
    case '}':
      if (delimiters.size() != 0 &&
          (delimiters.top() == '\'' || delimiters.top() == '"')) {
        continue;
      }
      if (delimiters.size() != 0 && delimiters.top() == openDelim[character]) {
        delimiters.pop();
        break;
      }
      return false;
    }
  }

  if (!delimiters.empty()) {
    return false;
  }

  return true;
}

int main() {
  cout << "Hello World!\n";
  vector<string> tests = {"()",
                          "",
                          "(",
                          ")",
                          "([{}])",
                          "()[}",
                          "c++ is good []{(()}",
                          "\"(\"",
                          "(\")))\")",
                          "'((('()"};

  for (const auto &test : tests) {
    cout << "Testing '" << test << "' " << isBalanced(test) << endl;
  }
}
