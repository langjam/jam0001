/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { readFileSync } from "fs"
import * as readline from "readline";
import { COMPInterpreter } from ".";
import { StackValue } from "./interpreter";

if (process.argv.length == 3) {
    let fileName = process.argv[2]
    let contents = readFileSync(fileName).toString()
    let interpreter = new COMPInterpreter()
    let results = interpreter.run(contents)

    console.log(`Running ${fileName}:`)
    console.log(contents)
    console.log("---")
    console.log(results)
} else if (process.argv.length == 2) {
    let stack: Array<StackValue> = new Array()
    console.log("Welcome to COMP!")
    console.log(`Stack: [${stack}]`)
    let interpreter = new COMPInterpreter()
    let prompt = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: "-> "
    })
    prompt.prompt()
    prompt.on('line', (line) => {
        interpreter.run(line, stack)
        console.log(`Stack: [${stack}]`)
        prompt.prompt()
    })
}
