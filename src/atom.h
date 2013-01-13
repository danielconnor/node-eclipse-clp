#ifndef ATOM_H
#define ATOM_H

#include <node.h>
#include "eclipse_h.h"

class Atom : public EC_atom, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static void Init(v8::Handle<v8::Object> target);

 private:
  Atom(char *s) : EC_atom(s) {};

  static v8::Handle<v8::Value> New(const v8::Arguments& args);
  static v8::Handle<v8::Value> getName(v8::Local<v8::String> property, const v8::AccessorInfo &info);
};

#endif
