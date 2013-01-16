
#include <node.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>

#include "eclipse_h.h"

#include "functor.h"
#include "atom.h"
#include "compound.h"


#include "ref.h"

using namespace v8;
using namespace node;


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

  EC_word *ec_args = new EC_word[args.Length() - 1];

  if(Functor::template_->HasInstance(args[0])) {

    Functor *functor = ObjectWrap::Unwrap<Functor>(
          args[0]->ToObject());

    for(int i = 1; i < args.Length(); i++) {
      ec_args[i - 1] = jsToProlog(args[i]);
    }

    EC_word compound = term(*functor, *ec_args);

    delete[] ec_args;

    return scope.Close(Compound::NewInstance(compound));
  }

  return scope.Close(Undefined());
}

Handle<Value> post_goal(const Arguments& args) {
  HandleScope scope;

  if(args[0]->IsObject()) {
    post_goal(jsToProlog(args[0]));
  }
  else if(args[0]->IsString()) {
    post_goal(*String::Utf8Value(args[0]));
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

  target->Set(String::NewSymbol("term"),
    FunctionTemplate::New(term)->GetFunction());


  Functor::Init(target);
  Ref::Init(target);
  Atom::Init(target);
  Compound::Init(target);
}


NODE_MODULE(libeclipse, Init)
