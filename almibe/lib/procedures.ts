/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { COMPError } from ".";
import { StackValue } from "./interpreter";

export const procedures = {
    add: function(stack: Array<StackValue>) {
        let left = stack.pop();
        let right = stack.pop();
        if (left != undefined && right != undefined) {
            stack.push(left + right)
        } else {
            throw new COMPError("Could not add numbers.")
        }
    },
    sub: function(stack: Array<StackValue>) {
        let left = stack.pop();
        let right = stack.pop();
        if (left != undefined && right != undefined) {
            stack.push(left - right)
        } else {
            throw new COMPError("Could not subtract numbers.")
        }
    },
    mul: function(stack: Array<StackValue>) {
        let left = stack.pop();
        let right = stack.pop();
        if (left != undefined && right != undefined) {
            stack.push(left * right)
        } else {
            throw new COMPError("Could not multiply numbers.")
        }
    },
    div: function(stack: Array<StackValue>) {
        let left = stack.pop();
        let right = stack.pop();
        if (left != undefined && right != undefined) {
            stack.push(left / right)
        } else {
            throw new COMPError("Could not divide numbers.")
        }
    }
}
