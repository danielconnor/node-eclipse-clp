#include "ref.h"

using namespace v8;

Persistent<FunctionTemplate> Ref::template_;
Persistent<Function> Ref::constructor;

void Ref::Init(Handle<Object> target) {
  // Prepare constructor template
  Local<FunctionTemplate> tpl = FunctionTemplate::New(New);

  template_ = Persistent<FunctionTemplate>::New(tpl);
  template_->InstanceTemplate()->SetInternalFieldCount(2);
  template_->InstanceTemplate()->SetAccessor(String::NewSymbol("value"), getValue, setValue);
  // Prototype
  template_->PrototypeTemplate()->Set(String::NewSymbol("cutTo"),
      FunctionTemplate::New(cutTo)->GetFunction());

  constructor = Persistent<Function>::New(template_->GetFunction());
  target->Set(String::NewSymbol("Ref"), constructor);
}

Handle<Value> Ref::New(const Arguments& args) {
  HandleScope scope;

  Ref* ref = new Ref();
  ref->Wrap(args.This());

  return scope.Close(args.This());
}

Handle<Value> Ref::NewInstance(EC_word &word) {
  HandleScope scope;

  Local<Object> instance = constructor->NewInstance(0, NULL);
  Ref* ref = ObjectWrap::Unwrap<Ref>(instance);

  ec_refs_set(ref->r, 0, word.w);

  return scope.Close(instance);
}

Handle<Value> Ref::getValue(Local<String> property, const AccessorInfo &info) {
  HandleScope scope;

  Ref* ref = ObjectWrap::Unwrap<Ref>(info.Holder());

  return scope.Close(prologToJS(EC_word(*ref)));
}

void Ref::setValue(Local<String> property, Local<Value> value, const AccessorInfo& info) {

  Ref* ref = ObjectWrap::Unwrap<Ref>(info.Holder());

  ec_refs_set(ref->r, 0, jsToProlog(value).w);
}

Handle<Value> Ref::cutTo(const Arguments& args) {
  HandleScope scope;

  Ref* ref = ObjectWrap::Unwrap<Ref>(args.This());

  ref->cut_to();

  return scope.Close(Undefined());
}