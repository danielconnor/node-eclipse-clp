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
var gt = new eclipse.Functor(">", 2);
var minus = new eclipse.Functor("-", 2);
var eq = new eclipse.Functor("=", 2);
var mul = new eclipse.Functor("*", 2);
var and = new eclipse.Functor("and", 2);

var l = [A, B];

eclipse.post_goal(eclipse.term(writeln, A));

eclipse.post_goal(eclipse.term(lib, "ptc_solver"));

eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__clean_up, ptc_solver__default_declarations"));

var condition = eclipse.term(and,
  eclipse.term(gt, A, 45),
  eclipse.term(eq,
    eclipse.term(minus, B, 5),
    eclipse.term(mul, A, A))
  );

console.log(condition);

eclipse.post_goal(eclipse.term(variable, l, integer));
eclipse.post_goal(eclipse.term(sdl, condition));
eclipse.post_goal(eclipse.term(label_integers, l));


while(0 == eclipse.resume()) {
  console.log("A:", A.value);
  console.log("B:", B.value);
  eclipse.post_goal(fail);
}


console.log(ec_status[eclipse.cleanup()]);

eclipse.status = ec_status;

module.exports = eclipse;

