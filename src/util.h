#ifndef UTIL_H
#define UTIL_H

#include <node.h>
#include "eclipse_h.h"
#include "ref.h"
#include "atom.h"
#include "functor.h"
#include "compound.h"
#include <iostream>


v8::Handle<v8::Value> prologToJS(EC_word);
EC_word jsToProlog(v8::Handle<v8::Value>);

#endif

