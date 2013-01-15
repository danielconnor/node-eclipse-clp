#ifndef FUNCTOR_H
#define FUNCTOR_H

#include <node.h>
#include "eclipse_h.h"

class Functor : public EC_functor, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static v8::Persistent<v8::Function> constructor;

  static void Init(v8::Handle<v8::Object>);
  static v8::Handle<v8::Value> NewInstance(char *, unsigned int);
  static v8::Handle<v8::Value> NewInstance(EC_functor *);

 private:

  Functor(char *name, int arity) : EC_functor(name, arity) {}

  static v8::Handle<v8::Value> New(const v8::Arguments&);
  static v8::Handle<v8::Value> getName(v8::Local<v8::String>, const v8::AccessorInfo&);
  static v8::Handle<v8::Value> getArity(v8::Local<v8::String>, const v8::AccessorInfo&);

};

#endif

