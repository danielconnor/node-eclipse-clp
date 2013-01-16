#include "compound.h"


using namespace v8;
using namespace std;

Persistent<FunctionTemplate> Compound::template_;
Persistent<Function> Compound::constructor;

void Compound::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Compound"));
  template_->InstanceTemplate()->SetInternalFieldCount(10);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("functor"), getFunctor);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("arity"), getArity);

  template_->InstanceTemplate()->SetIndexedPropertyHandler(getArg, 0, 0, 0, getArgs);

  constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Compound"), constructor);
}

Handle<Value> Compound::New(const Arguments& args) {
  HandleScope scope;

  Compound* compound = new Compound();
  compound->Wrap(args.This());

  return args.This();
}

Handle<Value> Compound::NewInstance(EC_word &word) {
  HandleScope scope;

  Local<Object> instance = constructor->NewInstance(0, NULL);
  Compound* compound = ObjectWrap::Unwrap<Compound>(instance);

  compound->w = word.w;

  return scope.Close(instance);
}

Handle<Value> Compound::getFunctor(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;
  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());

  EC_functor f;

  if(EC_succeed == compound->functor(&f)) {
    return scope.Close(Functor::NewInstance(&f));
  }
  else {
    return scope.Close(Undefined());
  }

}

Handle<Value> Compound::getArity(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());

  return scope.Close(Number::New(compound->arity()));
}

Handle<Array> Compound::getArgs(const AccessorInfo &info) {
  HandleScope scope;

  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());
  int arity = compound->arity();
  Handle<Array> items = Array::New(arity);

  for(int i = 1; i <= arity; i++) {
    items->Set(i - 1, Number::New(i));
  }

  return scope.Close(items);
}


Handle<Value> Compound::getArg(unsigned int index, const AccessorInfo &info) {
  HandleScope scope;

  if(index == 0) {
    return scope.Close(getFunctor(String::New("functor"), info));
  }

  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());

  if(index <= compound->arity())
  {
    EC_word arg;

    if(EC_succeed == compound->arg(index, arg)) {
      return scope.Close(prologToJS(arg));
    }
  }

  return scope.Close(Undefined());
}


