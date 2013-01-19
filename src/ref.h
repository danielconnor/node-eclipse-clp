#ifndef REF_H
#define REF_H

#include <node.h>
#include "eclipse_h.h"
#include "util.h"


class Ref : public EC_ref, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static v8::Persistent<v8::Function> constructor;
  static void Init(v8::Handle<v8::Object> target);

  static v8::Handle<v8::Value> NewInstance(EC_word &word);

 private:

  Ref() : EC_ref(), ObjectWrap() {}

  static v8::Handle<v8::Value> New(const v8::Arguments& args);
  static v8::Handle<v8::Value> cutTo(const v8::Arguments& args);
  static v8::Handle<v8::Value> getValue(v8::Local<v8::String>, const v8::AccessorInfo&);
  static void setValue(v8::Local<v8::String>, v8::Local<v8::Value>, const v8::AccessorInfo&);
};

#endif

