#ifndef COMPOUND_H
#define COMPOUND_H

#include <node.h>
#include "eclipse_h.h"
#include "functor.h"
#include "util.h"


class Compound : public EC_word, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static v8::Persistent<v8::Function> constructor;
  static void Init(v8::Handle<v8::Object>);
  static v8::Handle<v8::Value> NewInstance(EC_word&);

  Compound() : EC_word(), ObjectWrap() {}
 private:

  static v8::Handle<v8::Value> New(const v8::Arguments&);
  static v8::Handle<v8::Array> getArgs(const v8::AccessorInfo&);
  static v8::Handle<v8::Value> getArg(uint32_t, const v8::AccessorInfo&);
  static v8::Handle<v8::Value> getFunctor(v8::Local<v8::String>, const v8::AccessorInfo&);
  static v8::Handle<v8::Value> getArity(v8::Local<v8::String>, const v8::AccessorInfo&);
};


#endif