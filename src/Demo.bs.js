// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var DDD$ReasonPlayground = require("./DDD.bs.js");

console.log("Hello, BuckleScript and Reason!");

DDD$ReasonPlayground.axioms(DDD$ReasonPlayground.addToCart, "iPhoneX", "iPad", "Mac");

var ageAndName = /* tuple */[
  33,
  "Gabriel"
];

var me = /* record */[
  /* age */5,
  /* name */"Big Reason"
];

exports.ageAndName = ageAndName;
exports.me = me;
/*  Not a pure module */
