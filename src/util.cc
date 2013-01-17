#include "util.h"

using namespace node;
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
      // cout << "functor \n";
    }
  }

  if(EC_succeed == did.is_long(&long_val))
  {
    return scope.Close(Number::New(long_val));
  }

  if(EC_succeed == did.is_double(&double_val))
  {
    return scope.Close(Number::New(double_val));
  }

  if(EC_succeed == did.is_string(&string_val))
  {
    return scope.Close(String::New(string_val));
  }

  if(EC_succeed == did.is_list(head, tail))
  {
    Local<Array> list = Array::New(1);

    list->Set(v8::Number::New(0), prologToJS(head));

    for(int i = 1; (EC_succeed == tail.is_list(head, tail)); i++)
    {
      list->Set(i, prologToJS(head));
    }
    return scope.Close(list);
  }

  if(EC_succeed == did.functor(&functor_val))
  {
    return scope.Close(Compound::NewInstance(did));
  }

  return scope.Close(Undefined());
}


EC_word jsToProlog(Handle<Value> value) {

  if(value->IsString()) {
    return EC_word(*String::Utf8Value(value));
  }

  if(value->IsArray()) {
    Handle<Array> array = Handle<Array>::Cast(value);;

    EC_word word_list(ec_nil());

    for(unsigned i = 0; i < array->Length(); i++) {
      word_list = list(jsToProlog(array->Get(i)), word_list);
    }

    return word_list;
  }

  if(value->IsNumber()) {

    if(value->IsInt32()) {
      return EC_word(value->Int32Value());
    }
    else {
      return EC_word(value->NumberValue());
    }
  }

  if(value->IsObject()) {

    if(Atom::template_->HasInstance(value)) {
      return EC_word(*ObjectWrap::Unwrap<Atom>(value->ToObject()));
    }

    if(Ref::template_->HasInstance(value)) {
      return EC_word(*ObjectWrap::Unwrap<Ref>(value->ToObject()));
    }

    if(Functor::template_->HasInstance(value)) {
      // return EC_word(*ObjectWrap::Unwrap<Functor>(value->ToObject()));
    }

    if(Compound::template_->HasInstance(value)) {
      return EC_word(*ObjectWrap::Unwrap<Compound>(value->ToObject()));
    }

  }

  return EC_word();
}