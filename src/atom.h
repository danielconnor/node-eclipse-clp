#ifndef ATOM_H
#define ATOM_H

#include <node.h>
#include "eclipse_h.h"

class Atom : public EC_atom, public node::ObjectWrap {
 public:
  static v8::Persistent<v8::FunctionTemplate> template_;
  static v8::Persistent<v8::Function> constructor;
  static void Init(v8::Handle<v8::Object>);
  static v8::Handle<v8::Value> NewInstance(char *);
  static v8::Handle<v8::Value> NewInstance(EC_word&) {

 private:
  Atom(char *s) : EC_atom(s), ObjectWrap() {};

  static v8::Handle<v8::Value> New(const v8::Arguments&);
  static v8::Handle<v8::Value> getName(v8::Local<v8::String>, const v8::AccessorInfo&);
};

#endif
