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

Handle<Value> Compound::getArg(const v8::Arguments& args) {
  HandleScope scope;

  Compound* compound = ObjectWrap::Unwrap<Compound>(args.This());

  if(!args[0]->IsInt32())
  {
    return scope.Close(Undefined());
  }

  EC_word arg;

  if(EC_succeed == compound->arg(args[0]->Int32Value(), arg)) {
    return scope.Close(prologToJS(arg));
  }

  return scope.Close(Undefined());
}


