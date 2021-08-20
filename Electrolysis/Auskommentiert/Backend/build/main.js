"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var grammar_1 = require("./grammar");
var res = grammar_1.parse("rotate 2 comments above clockwise");
for (var _i = 0, _a = res.errs; _i < _a.length; _i++) {
    var e = _a[_i];
    console.log(e.toString());
}
console.log(JSON.stringify(res));
