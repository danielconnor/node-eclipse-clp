#include "util.h"

using namespace v8;
using namespace std;

Handle<Value> prologToJS(EC_word did)
{
  HandleScope scope;

  EC_atom atom_val;
  EC_functor functor_val;
  long long_val;
  double double_val;
  char *string_val;
  EC_functor val;

  EC_word head;
  EC_word tail;

  if(EC_succeed == did.is_atom(&atom_val))
  {
    int arity = did.arity();
    if(arity > 0) {
      cout << "atom :" << atom_val.name() << "\n";
      return scope.Close(Atom::NewInstance(atom_val));
    }
    else {
      cout << "functor \n";
    }
  }

  if(EC_succeed == did.is_long(&long_val))
  {
    cout << "long :" << long_val << "\n";
    return scope.Close(Number::New(long_val));
  }

  if(EC_succeed == did.is_double(&double_val))
  {
    cout << "double :" << double_val << "\n";
    return scope.Close(Number::New(double_val));
  }

  if(EC_succeed == did.is_string(&string_val))
  {
    cout << "string :" << string_val << "\n";
    return scope.Close(String::New(string_val));
  }

  if(EC_succeed == did.is_list(head, tail))
  {
    cout << "list\n";
    Local<Array> list = v8::Array::New(1);

    list->Set(v8::Number::New(0), prologToJS(head));

    for(int i = 1; (EC_succeed == tail.is_list(head, tail)); i++)
    {
      list->Set(i, prologToJS(head));
    }
    return scope.Close(list);
  }

  if(EC_succeed == did.functor(&functor_val))
  {
    cout << "compound\n";
    return scope.Close(Compound::NewInstance(did));
  }

  return scope.Close(Undefined());
}
