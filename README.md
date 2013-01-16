node-eclipse-clp
================

# Introduction
This is a binding to the ECLiPSe Constraint programming system for javascript. It interfaces with prolog using the [C++
interface](http://eclipseclp.org/doc/embedding/embroot065.html) provided by ECLiPSe.

# Setting up
- Install node.js - [http://nodejs.org/](http://nodejs.org/)
- Install ECLiPSe Constraint programming system - [http://eclipseclp.org/](http://eclipseclp.org/)
- The path to eclipse.dll must be available on your path. This is located at `eclipse_installation_path/lib/i386_nt`
- Get the source code `git clone git://github.com/danielconnor/node-eclipse-clp.git`
- Set up and build the module `npm install`

# Usage
```
var
  // load the eclipse module
  eclipse = require("node-eclipse-clp"),
  assert = require("assert");


// initialse the prolog environment and make sure it succeeds
assert.equal(eclipse.init(), eclipse.status.EC_succeed);

// create a reference to a new functor
var writeln = new eclipse.Functor("writeln", 1);

// create a compound term consisting of the functor, writeln, and the argument which is a string.
var compound = eclipse.term(writeln, "hello world");

eclipse.post_goal(compound);

eclipse.resume();

// cleanup the prolog environment
eclipse.cleanup();
```


