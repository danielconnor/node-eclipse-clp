#ifndef FUNCTOR_H
#define FUNCTOR_H

#include <node.h>
#include "eclipse_h.h"

class Functor : public EC_functor, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static void Init(v8::Handle<v8::Object> target);

 private:

  Functor(char *name, int arity) : EC_functor(name, arity) {}

  static v8::Handle<v8::Value> New(const v8::Arguments& args);
  static v8::Handle<v8::Value> getName(const v8::Arguments& args);
};

#endif

