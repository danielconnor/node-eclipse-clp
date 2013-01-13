#include <node.h>
#include "functor.h"

using namespace v8;

Persistent<FunctionTemplate> Functor::template_;


void Functor::Init(Handle<Object> target) {
  // Prepare constructor template

  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Functor"));
  template_->InstanceTemplate()->SetInternalFieldCount(2);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("name"), getName);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("arity"), getArity);

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


Handle<Value> Functor::getName(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Functor* obj = ObjectWrap::Unwrap<Functor>(info.Holder());

  return scope.Close(String::New(obj->name()));
}

Handle<Value> Functor::getArity(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Functor* obj = ObjectWrap::Unwrap<Functor>(info.Holder());

  return scope.Close(Number::New(obj->arity()));
}