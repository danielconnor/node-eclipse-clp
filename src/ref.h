#ifndef REF_H
#define REF_H

#include <node.h>
#include "eclipse_h.h"


class Ref : public EC_ref, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static void Init(v8::Handle<v8::Object> target);

 private:

  Ref() : EC_ref() {}

  static v8::Handle<v8::Value> New(const v8::Arguments& args);
  static v8::Handle<v8::Value> cutTo(const v8::Arguments& args);
  static v8::Handle<v8::Value> getValues(const v8::Arguments& args);
};

#endif

