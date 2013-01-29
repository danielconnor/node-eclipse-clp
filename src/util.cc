#include "util.h"

using namespace node;
using namespace v8;
using namespace std;

Handle<Value> prologToJS(EC_word &did)
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

  if(EC_succeed == did.is_var())
  {
    return scope.Close(Ref::NewInstance(did));
  }

  if(EC_succeed == did.is_atom(&atom_val))
  {
    int arity = did.arity();
    if(arity > 0) {
      cout << "atom :" << atom_val.name() << "\n";
      return scope.Close(Atom::NewInstance(atom_val));
    }
    else {
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


bool jsToProlog(Handle<Value> value, EC_word& result) {

  if(value->IsString()) {
    result = EC_word(*String::Utf8Value(value));
    return true;
  }

  if(value->IsArray()) {
    Handle<Array> array = Handle<Array>::Cast(value);;

    EC_word word_list(ec_nil());

    for(unsigned i = 0; i < array->Length(); i++) {
      EC_word head;

      if(jsToProlog(array->Get(i), head)) {
        word_list = list(head, word_list);
      }
      else return false;
    }

    result = word_list;
    return true;
  }

  if(value->IsNumber()) {
    if(value->IsInt32()) {
      result = EC_word((long)value->Int32Value());
      return true;
    }
    else {
      result = EC_word((double)value->NumberValue());
      return true;
    }
  }

  if(value->IsObject()) {
    if(Atom::template_->HasInstance(value)) {
      result = EC_word(*ObjectWrap::Unwrap<Atom>(value->ToObject()));
      return true;
    }

    if(Ref::template_->HasInstance(value)) {
      result = EC_word(*ObjectWrap::Unwrap<Ref>(value->ToObject()));
      return true;
    }

    if(Functor::template_->HasInstance(value)) {
      result = EC_word(*ObjectWrap::Unwrap<Functor>(value->ToObject()));
      return true;
    }

    if(Compound::template_->HasInstance(value)) {
      result = EC_word(*ObjectWrap::Unwrap<Compound>(value->ToObject()));
      return true;
    }

    return false;
  }

  return false;
}


void ThrowInvalidArgException(int index, Handle<Value> value) {

  ThrowException(Exception::TypeError(
    String::Concat(
      String::New("arg:"),
      String::Concat(
        Number::New(index)->ToString(),
        String::Concat(
          String::New(": No prolog type available for: "),
          value->ToString()
        )
      )
    )
  ));

}