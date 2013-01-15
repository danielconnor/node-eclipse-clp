#include <node.h>
#include "atom.h"

using namespace v8;


Persistent<FunctionTemplate> Atom::template_;
Persistent<Function> Atom::constructor;

void Compound::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Compound"));
  template_->InstanceTemplate()->SetInternalFieldCount(1);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("functor"), getFunctor);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("arity"), getArity);


  tpl->PrototypeTemplate()->Set(String::NewSymbol("getArg"),
      FunctionTemplate::New(getArg)->GetFunction());

  constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Compound"), constructor);
}

Handle<Value> Compound::New(const Arguments& args) {
  HandleScope scope;

  Compound* compound = new Compound();
  compound->Wrap(args.This());

  return args.This();
}

Handle<Value> Compound::NewInstance(EC_word& word) {
  HandleScope scope;

  Local<Object> instance = constructor->NewInstance(0, NULL);
  Compound* compound = ObjectWrap::Unwrap<Compound>(instance);

  // We need to set the word after the object was created because
  // there is no way of passing an EC_word as a v8::Argument
  compound = word;

  return scope.Close(instance);
}

Handle<Value> Compound::getFunctor(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());

  return scope.Close(functor);
}

Handle<Value> Compound::getArg(const v8::Arguments& args) {
  HandleScope scope;

  Compound* compound = ObjectWrap::Unwrap<Compound>(info.Holder());

}


