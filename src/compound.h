#ifndef COMPOUND_H
#define COMPOUND_H

#include <node.h>
#include "eclipse_h.h"

class Compound : public EC_word, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static v8::Persistent<v8::Function> constructor;
  static void Init(v8::Handle<v8::Object> target);
  static v8::Handle<v8::Value> NewInstance(EC_word& word);

 private:
  Compound() : EC_word(), ObjectWrap() {}

  static v8::Handle<v8::Value> New(const v8::Arguments& args);
  static v8::Handle<v8::Value> getArg(const v8::Arguments& args);
  static v8::Handle<v8::Value> functor(v8::Local<v8::String> property, const v8::AccessorInfo &info);
};


#endif