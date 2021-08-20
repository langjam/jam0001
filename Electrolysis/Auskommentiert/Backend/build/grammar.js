"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.SyntaxErr = exports.parse = exports.Parser = exports.ASTKinds = void 0;
var ASTKinds;
(function (ASTKinds) {
    ASTKinds["start"] = "start";
    ASTKinds["Comment_1"] = "Comment_1";
    ASTKinds["Comment_2"] = "Comment_2";
    ASTKinds["Comment_3"] = "Comment_3";
    ASTKinds["_"] = "_";
    ASTKinds["LoopComment"] = "LoopComment";
    ASTKinds["ManipulationComment"] = "ManipulationComment";
    ASTKinds["ManipulationComment_$0"] = "ManipulationComment_$0";
    ASTKinds["ManipulationCommentBase_1"] = "ManipulationCommentBase_1";
    ASTKinds["ManipulationCommentBase_2"] = "ManipulationCommentBase_2";
    ASTKinds["ManipulationCommentBase_3"] = "ManipulationCommentBase_3";
    ASTKinds["ManipulationCommentSwap"] = "ManipulationCommentSwap";
    ASTKinds["ManipulationCommentMove"] = "ManipulationCommentMove";
    ASTKinds["ManipulationCommentRotate"] = "ManipulationCommentRotate";
    ASTKinds["ManipulationCommentRotate_$0_1"] = "ManipulationCommentRotate_$0_1";
    ASTKinds["ManipulationCommentRotate_$0_2"] = "ManipulationCommentRotate_$0_2";
    ASTKinds["CommentSelector"] = "CommentSelector";
    ASTKinds["CommentSelector_$0_1"] = "CommentSelector_$0_1";
    ASTKinds["CommentSelector_$0_2"] = "CommentSelector_$0_2";
    ASTKinds["AssignmentComment"] = "AssignmentComment";
    ASTKinds["VarName"] = "VarName";
    ASTKinds["Expression_1"] = "Expression_1";
    ASTKinds["Expression_2"] = "Expression_2";
    ASTKinds["$EOF"] = "$EOF";
})(ASTKinds = exports.ASTKinds || (exports.ASTKinds = {}));
var Parser = /** @class */ (function () {
    function Parser(input) {
        this.negating = false;
        this.memoSafe = true;
        this.pos = { overallPos: 0, line: 1, offset: 0 };
        this.input = input;
    }
    Parser.prototype.reset = function (pos) {
        this.pos = pos;
    };
    Parser.prototype.finished = function () {
        return this.pos.overallPos === this.input.length;
    };
    Parser.prototype.clearMemos = function () {
    };
    Parser.prototype.matchstart = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$comment;
            var $$res = null;
            if (true
                && ($scope$comment = _this.matchComment($$dpth + 1, $$cr)) !== null
                && _this.match$EOF($$cr) !== null) {
                $$res = { kind: ASTKinds.start, comment: $scope$comment };
            }
            return $$res;
        });
    };
    Parser.prototype.matchComment = function ($$dpth, $$cr) {
        var _this = this;
        return this.choice([
            function () { return _this.matchComment_1($$dpth + 1, $$cr); },
            function () { return _this.matchComment_2($$dpth + 1, $$cr); },
            function () { return _this.matchComment_3($$dpth + 1, $$cr); },
        ]);
    };
    Parser.prototype.matchComment_1 = function ($$dpth, $$cr) {
        return this.matchLoopComment($$dpth + 1, $$cr);
    };
    Parser.prototype.matchComment_2 = function ($$dpth, $$cr) {
        return this.matchManipulationComment($$dpth + 1, $$cr);
    };
    Parser.prototype.matchComment_3 = function ($$dpth, $$cr) {
        return this.matchAssignmentComment($$dpth + 1, $$cr);
    };
    Parser.prototype.match_ = function ($$dpth, $$cr) {
        var _this = this;
        return this.loop(function () { return _this.regexAccept(String.raw(templateObject_1 || (templateObject_1 = __makeTemplateObject(["(?: )"], ["(?: )"]))), $$dpth + 1, $$cr); }, true);
    };
    Parser.prototype.matchLoopComment = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$whileExpression;
            var $$res = null;
            if (true
                && _this.regexAccept(String.raw(templateObject_2 || (templateObject_2 = __makeTemplateObject(["(?:while)"], ["(?:while)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$whileExpression = _this.matchExpression($$dpth + 1, $$cr)) !== null
                && _this.regexAccept(String.raw(templateObject_3 || (templateObject_3 = __makeTemplateObject(["(?::)"], ["(?::)"]))), $$dpth + 1, $$cr) !== null) {
                $$res = { kind: ASTKinds.LoopComment, whileExpression: $scope$whileExpression };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationComment = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$base;
            var $scope$self;
            var $$res = null;
            if (true
                && ($scope$base = _this.matchManipulationCommentBase($$dpth + 1, $$cr)) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && (($scope$self = _this.matchManipulationComment_$0($$dpth + 1, $$cr)) || true)) {
                $$res = { kind: ASTKinds.ManipulationComment, base: $scope$base, self: $scope$self };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationComment_$0 = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $$res = null;
            if (true
                && _this.regexAccept(String.raw(templateObject_4 || (templateObject_4 = __makeTemplateObject(["(?:including)"], ["(?:including)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && _this.regexAccept(String.raw(templateObject_5 || (templateObject_5 = __makeTemplateObject(["(?:self)"], ["(?:self)"]))), $$dpth + 1, $$cr) !== null) {
                $$res = { kind: ASTKinds.ManipulationComment_$0, };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationCommentBase = function ($$dpth, $$cr) {
        var _this = this;
        return this.choice([
            function () { return _this.matchManipulationCommentBase_1($$dpth + 1, $$cr); },
            function () { return _this.matchManipulationCommentBase_2($$dpth + 1, $$cr); },
            function () { return _this.matchManipulationCommentBase_3($$dpth + 1, $$cr); },
        ]);
    };
    Parser.prototype.matchManipulationCommentBase_1 = function ($$dpth, $$cr) {
        return this.matchManipulationCommentSwap($$dpth + 1, $$cr);
    };
    Parser.prototype.matchManipulationCommentBase_2 = function ($$dpth, $$cr) {
        return this.matchManipulationCommentMove($$dpth + 1, $$cr);
    };
    Parser.prototype.matchManipulationCommentBase_3 = function ($$dpth, $$cr) {
        return this.matchManipulationCommentRotate($$dpth + 1, $$cr);
    };
    Parser.prototype.matchManipulationCommentSwap = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$target;
            var $$res = null;
            if (true
                && _this.regexAccept(String.raw(templateObject_6 || (templateObject_6 = __makeTemplateObject(["(?:swap)"], ["(?:swap)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$target = _this.matchCommentSelector($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.ManipulationCommentSwap, target: $scope$target };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationCommentMove = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$target;
            var $$res = null;
            if (true
                && _this.regexAccept(String.raw(templateObject_7 || (templateObject_7 = __makeTemplateObject(["(?:move)"], ["(?:move)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$target = _this.matchCommentSelector($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.ManipulationCommentMove, target: $scope$target };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationCommentRotate = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$target;
            var $scope$rotateDir;
            var $$res = null;
            if (true
                && _this.regexAccept(String.raw(templateObject_8 || (templateObject_8 = __makeTemplateObject(["(?:rotate)"], ["(?:rotate)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$target = _this.matchCommentSelector($$dpth + 1, $$cr)) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$rotateDir = _this.matchManipulationCommentRotate_$0($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.ManipulationCommentRotate, target: $scope$target, rotateDir: $scope$rotateDir };
            }
            return $$res;
        });
    };
    Parser.prototype.matchManipulationCommentRotate_$0 = function ($$dpth, $$cr) {
        var _this = this;
        return this.choice([
            function () { return _this.matchManipulationCommentRotate_$0_1($$dpth + 1, $$cr); },
            function () { return _this.matchManipulationCommentRotate_$0_2($$dpth + 1, $$cr); },
        ]);
    };
    Parser.prototype.matchManipulationCommentRotate_$0_1 = function ($$dpth, $$cr) {
        return this.regexAccept(String.raw(templateObject_9 || (templateObject_9 = __makeTemplateObject(["(?:clockwise)"], ["(?:clockwise)"]))), $$dpth + 1, $$cr);
    };
    Parser.prototype.matchManipulationCommentRotate_$0_2 = function ($$dpth, $$cr) {
        return this.regexAccept(String.raw(templateObject_10 || (templateObject_10 = __makeTemplateObject(["(?:counterclockwise)"], ["(?:counterclockwise)"]))), $$dpth + 1, $$cr);
    };
    Parser.prototype.matchCommentSelector = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$count;
            var $scope$dir;
            var $$res = null;
            if (true
                && ($scope$count = _this.regexAccept(String.raw(templateObject_11 || (templateObject_11 = __makeTemplateObject(["(?:[0-9]+)"], ["(?:[0-9]+)"]))), $$dpth + 1, $$cr)) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && _this.regexAccept(String.raw(templateObject_12 || (templateObject_12 = __makeTemplateObject(["(?:comments)"], ["(?:comments)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$dir = _this.matchCommentSelector_$0($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.CommentSelector, count: $scope$count, dir: $scope$dir };
            }
            return $$res;
        });
    };
    Parser.prototype.matchCommentSelector_$0 = function ($$dpth, $$cr) {
        var _this = this;
        return this.choice([
            function () { return _this.matchCommentSelector_$0_1($$dpth + 1, $$cr); },
            function () { return _this.matchCommentSelector_$0_2($$dpth + 1, $$cr); },
        ]);
    };
    Parser.prototype.matchCommentSelector_$0_1 = function ($$dpth, $$cr) {
        return this.regexAccept(String.raw(templateObject_13 || (templateObject_13 = __makeTemplateObject(["(?:above)"], ["(?:above)"]))), $$dpth + 1, $$cr);
    };
    Parser.prototype.matchCommentSelector_$0_2 = function ($$dpth, $$cr) {
        return this.regexAccept(String.raw(templateObject_14 || (templateObject_14 = __makeTemplateObject(["(?:below)"], ["(?:below)"]))), $$dpth + 1, $$cr);
    };
    Parser.prototype.matchAssignmentComment = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$varName;
            var $scope$rhsExpr;
            var $$res = null;
            if (true
                && ($scope$varName = _this.matchVarName($$dpth + 1, $$cr)) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && _this.regexAccept(String.raw(templateObject_15 || (templateObject_15 = __makeTemplateObject(["(?:is)"], ["(?:is)"]))), $$dpth + 1, $$cr) !== null
                && _this.match_($$dpth + 1, $$cr) !== null
                && ($scope$rhsExpr = _this.matchExpression($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.AssignmentComment, varName: $scope$varName, rhsExpr: $scope$rhsExpr };
            }
            return $$res;
        });
    };
    Parser.prototype.matchVarName = function ($$dpth, $$cr) {
        return this.regexAccept(String.raw(templateObject_16 || (templateObject_16 = __makeTemplateObject(["(?:[a-zA-Z]+[0-9a-zA-Z]*)"], ["(?:[a-zA-Z]+[0-9a-zA-Z]*)"]))), $$dpth + 1, $$cr);
    };
    Parser.prototype.matchExpression = function ($$dpth, $$cr) {
        var _this = this;
        return this.choice([
            function () { return _this.matchExpression_1($$dpth + 1, $$cr); },
            function () { return _this.matchExpression_2($$dpth + 1, $$cr); },
        ]);
    };
    Parser.prototype.matchExpression_1 = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$varName;
            var $$res = null;
            if (true
                && ($scope$varName = _this.matchVarName($$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.Expression_1, varName: $scope$varName };
            }
            return $$res;
        });
    };
    Parser.prototype.matchExpression_2 = function ($$dpth, $$cr) {
        var _this = this;
        return this.run($$dpth, function () {
            var $scope$num;
            var $$res = null;
            if (true
                && ($scope$num = _this.regexAccept(String.raw(templateObject_17 || (templateObject_17 = __makeTemplateObject(["(?:[0-9]+)"], ["(?:[0-9]+)"]))), $$dpth + 1, $$cr)) !== null) {
                $$res = { kind: ASTKinds.Expression_2, num: $scope$num };
            }
            return $$res;
        });
    };
    Parser.prototype.test = function () {
        var mrk = this.mark();
        var res = this.matchstart(0);
        var ans = res !== null;
        this.reset(mrk);
        return ans;
    };
    Parser.prototype.parse = function () {
        var mrk = this.mark();
        var res = this.matchstart(0);
        if (res)
            return { ast: res, errs: [] };
        this.reset(mrk);
        var rec = new ErrorTracker();
        this.clearMemos();
        this.matchstart(0, rec);
        var err = rec.getErr();
        return { ast: res, errs: err !== null ? [err] : [] };
    };
    Parser.prototype.mark = function () {
        return this.pos;
    };
    Parser.prototype.loop = function (func, star) {
        if (star === void 0) { star = false; }
        var mrk = this.mark();
        var res = [];
        for (;;) {
            var t = func();
            if (t === null) {
                break;
            }
            res.push(t);
        }
        if (star || res.length > 0) {
            return res;
        }
        this.reset(mrk);
        return null;
    };
    Parser.prototype.run = function ($$dpth, fn) {
        var mrk = this.mark();
        var res = fn();
        if (res !== null)
            return res;
        this.reset(mrk);
        return null;
    };
    Parser.prototype.choice = function (fns) {
        for (var _i = 0, fns_1 = fns; _i < fns_1.length; _i++) {
            var f = fns_1[_i];
            var res = f();
            if (res !== null) {
                return res;
            }
        }
        return null;
    };
    Parser.prototype.regexAccept = function (match, dpth, cr) {
        var _this = this;
        return this.run(dpth, function () {
            var reg = new RegExp(match, "y");
            var mrk = _this.mark();
            reg.lastIndex = mrk.overallPos;
            var res = _this.tryConsume(reg);
            if (cr) {
                cr.record(mrk, res, {
                    kind: "RegexMatch",
                    // We substring from 3 to len - 1 to strip off the
                    // non-capture group syntax added as a WebKit workaround
                    literal: match.substring(3, match.length - 1),
                    negated: _this.negating,
                });
            }
            return res;
        });
    };
    Parser.prototype.tryConsume = function (reg) {
        var res = reg.exec(this.input);
        if (res) {
            var lineJmp = 0;
            var lind = -1;
            for (var i = 0; i < res[0].length; ++i) {
                if (res[0][i] === "\n") {
                    ++lineJmp;
                    lind = i;
                }
            }
            this.pos = {
                overallPos: reg.lastIndex,
                line: this.pos.line + lineJmp,
                offset: lind === -1 ? this.pos.offset + res[0].length : (res[0].length - lind - 1)
            };
            return res[0];
        }
        return null;
    };
    Parser.prototype.noConsume = function (fn) {
        var mrk = this.mark();
        var res = fn();
        this.reset(mrk);
        return res;
    };
    Parser.prototype.negate = function (fn) {
        var mrk = this.mark();
        var oneg = this.negating;
        this.negating = !oneg;
        var res = fn();
        this.negating = oneg;
        this.reset(mrk);
        return res === null ? true : null;
    };
    Parser.prototype.memoise = function (rule, memo) {
        var $scope$pos = this.mark();
        var $scope$memoRes = memo.get($scope$pos.overallPos);
        if (this.memoSafe && $scope$memoRes !== undefined) {
            this.reset($scope$memoRes[1]);
            return $scope$memoRes[0];
        }
        var $scope$result = rule();
        if (this.memoSafe)
            memo.set($scope$pos.overallPos, [$scope$result, this.mark()]);
        return $scope$result;
    };
    Parser.prototype.match$EOF = function (et) {
        var res = this.finished() ? { kind: ASTKinds.$EOF } : null;
        if (et)
            et.record(this.mark(), res, { kind: "EOF", negated: this.negating });
        return res;
    };
    return Parser;
}());
exports.Parser = Parser;
function parse(s) {
    var p = new Parser(s);
    return p.parse();
}
exports.parse = parse;
var SyntaxErr = /** @class */ (function () {
    function SyntaxErr(pos, expmatches) {
        this.pos = pos;
        this.expmatches = __spreadArray([], expmatches);
    }
    SyntaxErr.prototype.toString = function () {
        return "Syntax Error at line " + this.pos.line + ":" + this.pos.offset + ". Expected one of " + this.expmatches.map(function (x) { return x.kind === "EOF" ? " EOF" : " " + (x.negated ? 'not ' : '') + "'" + x.literal + "'"; });
    };
    return SyntaxErr;
}());
exports.SyntaxErr = SyntaxErr;
var ErrorTracker = /** @class */ (function () {
    function ErrorTracker() {
        this.mxpos = { overallPos: -1, line: -1, offset: -1 };
        this.regexset = new Set();
        this.pmatches = [];
    }
    ErrorTracker.prototype.record = function (pos, result, att) {
        if ((result === null) === att.negated)
            return;
        if (pos.overallPos > this.mxpos.overallPos) {
            this.mxpos = pos;
            this.pmatches = [];
            this.regexset.clear();
        }
        if (this.mxpos.overallPos === pos.overallPos) {
            if (att.kind === "RegexMatch") {
                if (!this.regexset.has(att.literal))
                    this.pmatches.push(att);
                this.regexset.add(att.literal);
            }
            else {
                this.pmatches.push(att);
            }
        }
    };
    ErrorTracker.prototype.getErr = function () {
        if (this.mxpos.overallPos !== -1)
            return new SyntaxErr(this.mxpos, this.pmatches);
        return null;
    };
    return ErrorTracker;
}());
var templateObject_1, templateObject_2, templateObject_3, templateObject_4, templateObject_5, templateObject_6, templateObject_7, templateObject_8, templateObject_9, templateObject_10, templateObject_11, templateObject_12, templateObject_13, templateObject_14, templateObject_15, templateObject_16, templateObject_17;
