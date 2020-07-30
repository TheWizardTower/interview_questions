/*
Interpret and evaluate arithmetic expressions written in plain English, like "one plus two times four"
- numbers are [zero-ten]
- numbers can be negative, for example "negative five"
- "plus" and "times" are the only supported operations
- natural order of operations apply, multiply before add.
- "negative" is optional string before the number and is not an operation
  - "one minus two" is expressed as "one plus negative two"
You are not allowed to use 
- eval function that is built in the language
- macros or compiler internals
- external interpreter

eval_("one plus one") == 2
eval_("five plus two times negative one") == 3
eval_("negative three times ten") == -30
eval_("five times three plus one") == 16
eval_("two times three times four") == 24
eval_("two times three plus four times five") == 26
eval_("two plus three times four plus five") == 19
eval_("two times three times four plus five times six") == 54
eval_("two times three plus four plus five plus six times seven") == 57

*/
#include <iostream>
#include <sstream>
#include <jsoncpp/json/json.h>
#include <assert.h>
#include <vector>
#include <iterator>
#include <variant>

using std::cout;
using std::endl;
using std::get_if;
using std::istream_iterator;
using std::istringstream;
using std::string;
using std::variant;
using std::vector;

enum class Operation
{
  Negative,
  Times,
  Plus
};

struct MyExpressionTree
{
  Operation operation;
  variant<int, MyExpressionTree *> LeftOperand;
  variant<int, MyExpressionTree *> RightOperand;
};

int eval_(string expression)
{
  istringstream iss(expression);
  vector<string> tokens((istream_iterator<string>{iss}), istream_iterator<string>());

  vector<variant<int, Operation>> ast;

  for (const auto token : tokens)
  {
    cout << token << endl;
    if (token == "zero")
    {
      ast.push_back(0);
    }
    else if (token == "one")
    {
      ast.push_back(1);
    }
    else if (token == "two")
    {
      ast.push_back(2);
    }
    else if (token == "three")
    {
      ast.push_back(3);
    }
    else if (token == "four")
    {
      ast.push_back(4);
    }
    else if (token == "five")
    {
      ast.push_back(5);
    }
    else if (token == "six")
    {
      ast.push_back(6);
    }
    else if (token == "seven")
    {
      ast.push_back(7);
    }
    else if (token == "eight")
    {
      ast.push_back(8);
    }
    else if (token == "nine")
    {
      ast.push_back(9);
    }
    else if (token == "ten")
    {
      ast.push_back(10);
    }
    else if (token == "negative")
    {
      ast.push_back(Operation::Negative);
    }
    else if (token == "times")
    {
      ast.push_back(Operation::Times);
    }
    else if (token == "plus")
    {
      ast.push_back(Operation::Plus);
    }
  }

  for (const auto node : ast)
  {
    auto oper = get_if<Operation>(&node);
    if (oper != NULL)
    {
      if (*oper == Operation::Negative)
      {
        cout << "Negative operation." << endl;
      }
      else if (*oper == Operation::Times)
      {
        cout << "Times operation." << endl;
      }
      else if (*oper == Operation::Plus)
      {
        cout << "Plus operation." << endl;
      }
    }
    auto literal = get_if<int>(&node);
    if (literal != NULL)
    {
      cout << "Number: " << *literal << endl;
    }
  }

  vector<variant<int, Operation>> astPassOne;

  for (int i = 0; i < ast.size(); ++i)
  {
    auto oper = get_if<Operation>(&ast[i]);
    if (oper != NULL)
    {
      if (*oper == Operation::Negative)
      {
        cout << "Negative operation." << endl;
        if (i + 1 < ast.size())
        {
          cout << "Missing operand." << endl;
          break;
        }
        else
        {
          auto literal = get_if<int>(&ast[i + 1]);
          if (literal == NULL)
          {
            cout << "Invalid expression" << endl;
            break;
          }
          astPassOne.push_back(-1 * *literal);
          ++i;
          break;
        }
      }
      astPassOne.push_back(ast[i]);
    }
  }
  return 0;
}
int main()
{
  std::cout << "Hello world" << std::endl;

  assert(eval_("one plus one") == 2);
  assert(eval_("five plus two times negative one") == 3);
  assert(eval_("negative three times ten") == -30);
  assert(eval_("five times three plus one") == 16);
  assert(eval_("two times three times four") == 24);
  assert(eval_("two times three plus four times five") == 26);
  assert(eval_("two plus three times four plus five") == 19);
  assert(eval_("two times three times four plus five times six") == 54);
  assert(eval_("two times three plus four plus five plus six times seven") == 57);
  return 0;
}
