"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var parser0001_1 = require("./parse-that/parser0001");
var usage_str = "Usage: node purr 'your purr program here'";
var purr = function () {
    // console.log("Purr\n");
    // console.log(process.argv);
    if (!process.argv[2]) {
        console.error("no input passed to purr\n", usage_str);
        return 1;
    }
    var purr_input = process.argv[2].replace(/\\n/g, '\n ');
    var parsed_purr = parser0001_1.parse(purr_input);
    // while (parsed_purr) {
    //     const word = parsed_purr.shift();
    //     // if (word && dictionary[word]) {
    //     // }
    // }
};
purr();
//# sourceMappingURL=purr.js.map