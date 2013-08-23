#include <node.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>

#include "eclipse_h.h"
#include "functor.h"
#include "atom.h"
#include "compound.h"
#include "ref.h"
#include "util.h"

using namespace v8;
using namespace node;
using namespace std;

Handle<Value> init(const Arguments& args) {
  HandleScope scope;
  return scope.Close(Number::New(ec_init()));
}


Handle<Value> cleanup(const Arguments& args) {
  HandleScope scope;
  return scope.Close(Number::New(ec_cleanup()));
}


Handle<Value> resume(const Arguments& args) {
  HandleScope scope;
  int result;

  if(args.Length()) {
    Ref* ref = ObjectWrap::Unwrap<Ref>(
        args[0]->ToObject());

    result = EC_resume(*ref);
  }
  else {
    result = EC_resume();
  }

  return scope.Close(Number::New(result));
}


Handle<Value> term(const Arguments& args) {
  HandleScope scope;

  if(Functor::template_->HasInstance(args[0])) {
    EC_word *ec_args = new EC_word[args.Length() - 1];

    Functor *functor = ObjectWrap::Unwrap<Functor>(
          args[0]->ToObject());

    // convert all javascript values to prolog ones so they
    // can be passed to term.
    for(int i = 1; i < args.Length(); i++) {
      EC_word word;

      if(!jsToProlog(args[i], word)) {
        ThrowInvalidArgException(i, args[i]);
        delete[] ec_args;
        return scope.Close(Undefined());
      }

      ec_args[i - 1] = word;
    }

    EC_word compound = term(*functor, ec_args);

    delete[] ec_args;

    return scope.Close(Compound::NewInstance(compound));
  }

  return scope.Close(Undefined());
}


Handle<Value> post_goal(const Arguments& args) {
  HandleScope scope;

  if(args[0]->IsObject()) {
    EC_word word;

    if(!jsToProlog(args[0], word)) {

      ThrowInvalidArgException(0, args[0]);

      return scope.Close(Undefined());
    }

    post_goal(word);
  }
  else if(args[0]->IsString()) {
    post_goal(*String::Utf8Value(args[0]));
  }

  return scope.Close(Undefined());
}


void Init(Handle<Object> exports, Handle<Value> module) {

  NODE_SET_METHOD(exports, "init", init);
  NODE_SET_METHOD(exports, "cleanup", cleanup);
  NODE_SET_METHOD(exports, "resume", resume);
  NODE_SET_METHOD(exports, "post_goal", post_goal);
  NODE_SET_METHOD(exports, "term", term);

  Functor::Init(exports);
  Ref::Init(exports);
  Atom::Init(exports);
  Compound::Init(exports);
}


NODE_MODULE(libeclipse, Init)
