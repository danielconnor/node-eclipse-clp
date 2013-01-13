#include <node.h>
#include "ref.h"

using namespace v8;

Persistent<FunctionTemplate> Ref::template_;

void Ref::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->InstanceTemplate()->SetInternalFieldCount(1);
  // Prototype
  template_->PrototypeTemplate()->Set(String::NewSymbol("cutTo"),
      FunctionTemplate::New(cutTo)->GetFunction());

  Persistent<Function> constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Ref"), constructor);
}

Handle<Value> Ref::New(const Arguments& args) {
  HandleScope scope;

  Ref* ref = new Ref();
  ref->Wrap(args.This());

  return args.This();
}


Handle<Value> Ref::cutTo(const Arguments& args) {
  HandleScope scope;

  Ref* ref = ObjectWrap::Unwrap<Ref>(args.This());

  ref->cut_to();

  return scope.Close(Undefined());
}