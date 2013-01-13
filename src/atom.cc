#include <node.h>
#include "atom.h"

using namespace v8;


Persistent<FunctionTemplate> Atom::template_;

void Atom::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Atom"));
  template_->InstanceTemplate()->SetInternalFieldCount(1);
  // Prototype
  template_->PrototypeTemplate()->Set(String::NewSymbol("getName"),
      FunctionTemplate::New(getName)->GetFunction());

  Persistent<Function> constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Atom"), constructor);
}

Handle<Value> Atom::New(const Arguments& args) {
  HandleScope scope;

  Atom* atom = new Atom(*String::Utf8Value(args[0]));
  atom->Wrap(args.This());

  return args.This();
}

Handle<Value> Atom::getName(const Arguments& args) {
  HandleScope scope;

  Atom* atom = ObjectWrap::Unwrap<Atom>(args.This());

  return scope.Close(String::New(atom->name()));
}


