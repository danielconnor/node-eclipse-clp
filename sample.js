process.env["PATH"] += __dirname + "\\deps\\ECLiPSe 5.8\\lib\\i386_nt";
var eclipse = require("./build/Release/libeclipse");

var ec_status = {
  0: "EC_succeed",
  1: "EC_fail",
  2: "EC_throw",
  3: "EC_yield",
  4: "EC_running",
  5: "EC_waitio",
  6: "EC_flushio",
  EC_succeed: 0,
  EC_fail: 1,
  EC_throw: 2,
  EC_yield: 3,
  EC_running: 4,
  EC_waitio: 5,
  EC_flushio: 6
};

console.log(ec_status[eclipse.init()]);

var lib = new eclipse.Functor("lib", 1);
var submit_string = new eclipse.Functor("ptc_solver__submit_string", 1);
var writeln = new eclipse.Functor("writeln",1);

var pred = new eclipse.Ref();
var fail = new eclipse.Atom("fail");
var integer = new eclipse.Atom("integer");

var A = new eclipse.Ref();
var B = new eclipse.Ref();

var clean_up = new eclipse.Atom("ptc_solver__clean_up");
var default_declarations = new eclipse.Atom("ptc_solver__default_declarations");
var variable = new eclipse.Functor("ptc_solver__variable", 2);
var sdl = new eclipse.Functor("ptc_solver__sdl", 1);
var label_integers = new eclipse.Functor("ptc_solver__label_integers", 1);
var lt = new eclipse.Functor("<", 2);
var gt = new eclipse.Functor(">", 2);
var minus = new eclipse.Functor("-", 2);
var eq = new eclipse.Functor("=", 2);
var neq = new eclipse.Functor("\\=", 2);
var mul = new eclipse.Functor("*", 2);
var add = new eclipse.Functor("+", 2);
var and = new eclipse.Functor("and", 2);
var not = new eclipse.Functor("not", 1);
var comma = new eclipse.Functor(",", 2);
var exc = new eclipse.Atom("!");

var l = [A, B];
var solution = new eclipse.Ref();

eclipse.post_goal(eclipse.term(lib, "ptc_solver"));

eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__clean_up, ptc_solver__default_declarations"));

eclipse.post_goal(eclipse.term(variable, l, integer));
eclipse.post_goal(eclipse.term(eq, A, B));
console.log(eclipse.resume());
eclipse.post_goal(eclipse.term(sdl, eclipse.term(not, eclipse.term(eq, A, B))));
console.log(eclipse.resume());


eclipse.post_goal(eclipse.term(comma,
    eclipse.term(label_integers, l),
    exc
  )
);

var output = [];
var values = 0;
if(0 === eclipse.resume()) {
  console.log(l);
}

A.value = undefined;
B.value = undefined;

eclipse.post_goal(eclipse.term(variable, l, integer));
eclipse.post_goal(eclipse.term(sdl, eclipse.term(not, eclipse.term(eq, A, B))));
console.log(eclipse.resume());
eclipse.post_goal(eclipse.term(eq, A, B));
console.log(eclipse.resume());

eclipse.post_goal(eclipse.term(comma,
    eclipse.term(label_integers, l),
    exc
  )
);

if(0 === eclipse.resume()) {
  eclipse.resume();
  console.log(l);
}

console.log(ec_status[eclipse.cleanup()]);

eclipse.status = ec_status;


module.exports = eclipse;

