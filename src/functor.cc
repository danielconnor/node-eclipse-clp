#include <node.h>
#include "functor.h"

using namespace v8;

Persistent<FunctionTemplate> Functor::template_;


void Functor::Init(Handle<Object> target) {
  // Prepare constructor template

  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Functor"));
  template_->InstanceTemplate()->SetInternalFieldCount(1);
  // Prototype
  template_->PrototypeTemplate()->Set(String::NewSymbol("getName"),
      FunctionTemplate::New(getName)->GetFunction());

  Persistent<Function> constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Functor"), constructor);
}

Handle<Value> Functor::New(const Arguments& args) {
  HandleScope scope;

  if (!args[0]->IsString() || !args[1]->IsInt32()) {
    ThrowException(Exception::TypeError(String::New("Wrong arguments")));
    return scope.Close(Undefined());
  }

  Functor* functor = new Functor(*String::Utf8Value(args[0]), args[1]->Int32Value());
  functor->Wrap(args.This());

  return args.This();
}


Handle<Value> Functor::getName(const Arguments& args) {
  HandleScope scope;

  Functor* obj = ObjectWrap::Unwrap<Functor>(args.This());

  return String::New(obj->name());
}
