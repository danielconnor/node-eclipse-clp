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

eclipse.post_goal(new eclipse.Functor("current_built_in", 1), pred);
while (ec_status.EC_succeed == eclipse.resume())
{
    eclipse.post_goal(writeln, pred);
    eclipse.post_goal(fail);
}

console.log(ec_status[eclipse.cleanup()]);

eclipse.status = ec_status;

module.exports = eclipse;
