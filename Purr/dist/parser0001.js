"use strict";
// hello I am a very simple parser that does not really accomplish much
// , but little steps forward are better than a hot mess that does
// everything all in one go.
Object.defineProperty(exports, "__esModule", { value: true });
exports.parse = void 0;
var parse = function (just_the_purr_text_and_only_the_text) {
    var firstPass = just_the_purr_text_and_only_the_text.split(/[ |\t]+/);
    var after_mopping_str = mop_up_quoted_strings(firstPass);
    var after_mopping = mop_up_comments(after_mopping_str);
    return zap_newlines(after_mopping);
};
exports.parse = parse;
var mop_up_quoted_strings = function (mess) {
    var mopped_up = [];
    var i = 0;
    var in_str = false;
    var current_comment = "";
    while (mess[i]) {
        if (!in_str) {
            if (mess[i].length >= 1 && mess[i][0] === '"'
                && mess[i][mess[i].length - 1] !== '"') {
                in_str = true;
                current_comment = mess[i];
            }
            else {
                mopped_up.push(mess[i]);
            }
        }
        else {
            current_comment += " " + mess[i];
            if (mess[i].indexOf('"') >= 0) {
                mopped_up.push(current_comment);
                in_str = false;
                current_comment = "";
            }
        }
        i++;
    }
    // last comment in program that does nto end in a new-line
    if (current_comment) {
        console.log("last comment", current_comment);
        mopped_up.push(current_comment);
    }
    return mopped_up;
};
var mop_up_comments = function (mess) {
    var mopped_up = [];
    var i = 0;
    var in_comment = false;
    var current_comment = "";
    while (mess[i]) {
        if (!in_comment) {
            if (mess[i].length >= 1 && mess[i][0] === '#') {
                in_comment = true;
                current_comment = mess[i];
            }
            else {
                mopped_up.push(mess[i]);
            }
        }
        else {
            current_comment += " " + mess[i];
            if (mess[i].indexOf("\n") >= 0) {
                mopped_up.push(current_comment);
                in_comment = false;
                current_comment = "";
            }
        }
        i++;
    }
    // last comment in program that does nto end in a new-line
    if (current_comment) {
        console.log("last comment", current_comment);
        mopped_up.push(current_comment);
    }
    return mopped_up;
};
var zap_newlines = function (mess) {
    var zapped = [];
    var i = 0;
    while (mess[i]) {
        if (mess[i].indexOf("\n") >= 0) {
            zapped.push(mess[i].slice(0, -1));
        }
        else {
            zapped.push(mess[i]);
        }
        i++;
    }
    return zapped;
};
// or match on /(#[^\n]*\n)|(\w)/
//# sourceMappingURL=parser0001.js.map