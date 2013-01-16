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

eclipse.post_goal(eclipse.term(lib, "ptc_solver"));

eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__clean_up, ptc_solver__default_declarations"));
eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__variable([A, B], integer)"));
eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__sdl(A>45 and B-5=A*A)"));
eclipse.post_goal(eclipse.term(submit_string, "ptc_solver__label_integers([A,B])"));
eclipse.post_goal(eclipse.term(new eclipse.Functor("ptc_solver__get_all_variables", 1), pred));

eclipse.resume();

console.log(pred.getValues());

console.log(ec_status[eclipse.cleanup()]);

eclipse.status = ec_status;

module.exports = eclipse;
