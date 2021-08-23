"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.words = void 0;
exports.words = {
    '+': function (stack) {
        stack.push(stack.pop() + stack.pop());
        return stack;
    },
    'dup': function (stack) {
        var top = stack.pop();
        stack.push(top);
        stack.push(top);
        return stack;
    },
};
//# sourceMappingURL=words.js.map