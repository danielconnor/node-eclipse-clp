process.env["PATH"] += __dirname + "deps/ECLiPSe 5.8/lib/i386_nt";

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

eclipse.init();

eclipse.status = ec_status;
module.exports = eclipse;

