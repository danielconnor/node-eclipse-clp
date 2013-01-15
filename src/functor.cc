#include <node.h>
#include "functor.h"

using namespace v8;

Persistent<FunctionTemplate> Functor::template_;
Persistent<Function> Functor::constructor;


void Functor::Init(Handle<Object> target) {
  // Prepare constructor template

  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->SetClassName(String::NewSymbol("Functor"));
  template_->InstanceTemplate()->SetInternalFieldCount(2);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("name"), getName);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("arity"), getArity);

  constructor = Persistent<Function>::New(template_->GetFunction());
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

Handle<Value> Functor::NewInstance(char *name, unsigned int arity) {
  HandleScope scope;

  const unsigned argc = 2;
  Handle<Value> argv[argc] = {
    String::New(name),
    Number::New(arity)
  };
  Local<Object> instance = constructor->NewInstance(argc, argv);

  return scope.Close(instance);
}


Handle<Value> Functor::NewInstance(EC_word &functor) {
  return scope.Close(NewInstance(functor->name(), functor->arity()));
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
