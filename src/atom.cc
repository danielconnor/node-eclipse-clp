#include "atom.h"

using namespace v8;

Persistent<FunctionTemplate> Atom::template_;
Persistent<Function> Atom::constructor;

void Atom::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Atom"));
  template_->InstanceTemplate()->SetInternalFieldCount(1);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("name"), getName);

  constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Atom"), constructor);
}


Handle<Value> Atom::New(const Arguments& args) {
  HandleScope scope;

  Atom* atom = new Atom(*String::Utf8Value(args[0]));
  atom->Wrap(args.This());

  return args.This();
}


Handle<Value> Atom::NewInstance(char *name) {
  HandleScope scope;


  Handle<Value> argv[] = { String::New(name) };
  Local<Object> instance = constructor->NewInstance(1, argv);

  return scope.Close(instance);
}


Handle<Value> Atom::NewInstance(EC_atom& atom) {
  return NewInstance(atom.name());
}


Handle<Value> Atom::getName(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Atom* atom = ObjectWrap::Unwrap<Atom>(info.Holder());

  return scope.Close(String::New(atom->name()));
}


