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
#include <assert.h>
#include <vector>
#include <iterator>
#include <variant>
#include <string>

using std::cout;
using std::endl;
using std::get_if;
using std::istream_iterator;
using std::istringstream;
using std::ostream;
using std::string;
using std::to_string;
using std::variant;
using std::vector;

enum class Operation
{
  Negative,
  Times,
  Plus
};

string showASTValue(const variant<int, Operation> &val)
{
  string result;
  auto oper = get_if<Operation>(&val);
  if (oper != NULL)
  {
    if (*oper == Operation::Negative)
    {
      result = "Negative operation.";
    }
    else if (*oper == Operation::Times)
    {
      result = "Times operation.";
    }
    else if (*oper == Operation::Plus)
    {
      result = "Plus operation.";
    }
  }
  auto literal = get_if<int>(&val);
  if (literal != NULL)
  {
    result = "Number: " + to_string(*literal);
  }

  return result;
}

// Operator definition for the value of our AST vector, referencing the above case/switch
// function.
ostream &operator<<(ostream &os, const variant<int, Operation> &var)
{
  os << showASTValue(var);
  return os;
}

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

  cout << "Examining AST post tokenization." << endl;

  for (const auto node : ast)
  {
    cout << node << endl;
  }

  // Evaluate negative operator.
  for (auto iter = ast.begin(); iter != ast.end(); ++iter)
  {
    auto oper = get_if<Operation>(&(*iter));
    if (oper == NULL)
    {
      cout << "\tNegative operation, came across non-operator value." << endl;
      continue;
    }
    if (*oper != Operation::Negative)
    {
      cout << "\tNegative operation, came across non-negation operator." << endl;
      continue;
    }
    cout << "Negative operation." << endl;
    if (iter == ast.end())
    {
      cout << "Missing operand." << endl;
      break;
    }
    else
    {
      auto literal = get_if<int>(&(*(iter + 1)));
      if (literal == NULL)
      {
        cout << "Invalid expression" << endl;
        return -1;
      }
      auto result = (-1) * (*literal);
      cout << "Negation result: " << result << endl;
      iter = ast.erase(iter, iter + 2);
      iter = ast.emplace(iter, result);
    }
  }

  cout << "Out of negation evaluotian loop. Examining AST." << endl;
  for (const auto node : ast)
  {
    cout << "\t" << node << endl;
  }

  // Evaluate multiply
  for (auto iter = ast.begin(); iter != ast.end(); ++iter)
  {
    auto oper = get_if<Operation>(&(*(iter)));
    if (oper == NULL)
    {
      cout << "\tMultiply loop, found non-operator value, continuing over." << endl;
      continue;
    }
    if (*oper != Operation::Times)
    {
      cout << "\tMultiply loop, found non-times operator value, continuing over." << endl;
      continue;
    }
    if (iter == ast.begin())
    {
      cout << "Missing left operand on multiply operation." << endl;
    }
    auto leftOperand = get_if<int>(&(*(iter - 1)));
    if (leftOperand == NULL)
    {
      cout << "Invalid syntax on multiply operation." << endl;
    }
    if (iter == ast.end())
    {
      cout << "Missing right operand on multiply operation." << endl;
    }
    auto rightOperand = get_if<int>(&(*(iter + 1)));
    if (rightOperand == NULL)
    {
      cout << "Invalid syntax on multiply operation." << endl;
    }
    cout << "  Left operand: " << *leftOperand << endl;
    cout << "  Right operand: " << *rightOperand << endl;
    auto result = (*leftOperand) * (*rightOperand);
    cout << "Result: " << result << endl;
    iter = ast.erase(iter - 1, iter + 2);
    iter = ast.emplace(iter, result);
  }

  // Evaluate addition
  for (auto iter = ast.begin(); iter != ast.end(); ++iter)
  {
    cout << "  Entering addition loop." << endl;
    auto oper = get_if<Operation>(&(*(iter)));
    if (oper == NULL)
    {
      cout << "  Found a non-operator value, passing over." << endl;
      continue;
    }
    if (*oper != Operation::Plus)
    {
      cout << "  Not a plus operator." << endl;
      continue;
    }
    cout << "  Okay, found plus.";
    cout << "  Past gates." << endl;
    if (iter == ast.begin())
    {
      cout << "  Missing left operand on addition operation." << endl;
    }
    cout << "  Getting left operand." << endl;
    auto leftOperand = get_if<int>(&(*(iter - 1)));
    cout << "  Left operand gotten." << endl;
    if (leftOperand == NULL)
    {
      cout << "  Inavalid syntax on addition operation." << endl;
    }

    if (iter == ast.end())
    {
      cout << "  Missing right operand on addition operation." << endl;
    }
    cout << "  Getting right operand." << endl;
    auto rightOperand = get_if<int>(&(*(iter + 1)));
    cout << "  Right operand gotten." << endl;
    if (rightOperand == NULL)
    {
      cout << "  Inavalid syntax on addition operation." << endl;
    }
    cout << "  Left operand: " << *leftOperand << endl;
    cout << "  Right operand: " << *rightOperand << endl;
    auto result = *leftOperand + *rightOperand;
    cout << "Result: " << result << endl;
    iter = ast.erase(iter - 1, iter + 2);
    iter = ast.emplace(iter, result);
  }

  if (ast.size() != 1)
  {
    cout << "Something very unfortunate happened, the size isn't one." << endl;
  }
  auto finalResult = get_if<int>(&ast.at(0));
  if (finalResult == NULL)
  {
    cout << "Final result was not a number!";
  }
  cout << "About to return" << endl;
  cout << "Result: " << *finalResult << endl;
  return *finalResult;
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
