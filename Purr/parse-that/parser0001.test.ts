import { parse } from "./parser0001";

const tests = [
    [`well at least there is this`, ["well","at","least","there","is","this"]],
    [`this  is [ so ] simple`, ["this","is","[","so","]","simple"]],
    [`# OMG this caviare is not organically sourced!
    regular-vanilla-word-here
    # ^ an airline first-class comment (get it)`, ["# OMG this caviare is not organically sourced!", "regular-vanilla-word-here", "# ^ an airline first-class comment (get it)"]],
    [`"hello quoted strings" `, ['"hello quoted strings"']],
    [`"hello" word "there"`, ['"hello"', "word", '"there"']],
];

// ugh not another cheap object comparison
const objEq = (obj1: any[], obj2: any[]) => JSON.stringify(obj1) === JSON.stringify(obj2);


let all_good = true; // everything is going to be all right, in purr we trust.
console.log(`parse tests for purr! ${tests.length} tests started.\n`);

tests.map((test: any[]) => {
    if (!objEq(parse(test[0]), test[1])) {
        all_good = false;
        console.log(`A real purr kill! "${test[0]}" 
        failed our parse test, got this instead ${JSON.stringify(parse(test[0]))}
        \n`);
    }
});

if (all_good) {
    console.log(`----Purrrrrrrrfect----- all purr parse tests passed.\n`);    
}

