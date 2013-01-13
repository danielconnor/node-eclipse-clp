
#include <v8.h>
#include <node.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>

#include "eclipse_h.h"
#include "functor.h"
#include "atom.h"
#include "ref.h"



using namespace v8;
using namespace node;


Handle<Value> init(const Arguments& args) {
  HandleScope scope;

  ec_set_option_ptr(EC_OPTION_DEFAULT_MODULE, "ptc_solver");

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

Handle<Value> post_goal(const Arguments& args) {
  HandleScope scope;

  if(args[0]->IsObject()) {

    if(Functor::template_->HasInstance(args[0])) {

      Functor *functor = ObjectWrap::Unwrap<Functor>(
            args[0]->ToObject());

      if(args[1]->IsString()) {
        post_goal(term(*functor, EC_word(*String::Utf8Value(args[1]))));
      }
      else if(args[1]->IsObject()) {
        Ref *ref = ObjectWrap::Unwrap<Ref>(
          args[1]->ToObject());

        post_goal(term(*functor, *ref));
      }

    }
    else if(Atom::template_->HasInstance(args[0])) {
      Atom *atom = ObjectWrap::Unwrap<Atom>(
            args[0]->ToObject());

      post_goal(*atom);
    }

  }


  return scope.Close(Undefined());
}

void Init(Handle<Object> target) {
  target->Set(String::NewSymbol("init"),
      FunctionTemplate::New(init)->GetFunction());

  target->Set(String::NewSymbol("cleanup"),
      FunctionTemplate::New(cleanup)->GetFunction());

  target->Set(String::NewSymbol("resume"),
      FunctionTemplate::New(resume)->GetFunction());

  target->Set(String::NewSymbol("post_goal"),
    FunctionTemplate::New(post_goal)->GetFunction());

  Functor::Init(target);
  Ref::Init(target);
  Atom::Init(target);
}


NODE_MODULE(libeclipse, Init)
