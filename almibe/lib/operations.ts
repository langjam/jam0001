/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { debug, TODO } from "./debug";
import { StackValue } from "./interpreter";
import { procedures } from "./procedures";

export interface Operation {
    run(stack: Array<StackValue>): void
}

export class PushOperation implements Operation {
    readonly value: StackValue

    constructor(value: StackValue) {
        this.value = value
    }

    run(stack: StackValue[]): void {
        stack.push(this.value)
    }
}

export class PopOperation implements Operation {
    run(stack: StackValue[]): void {
        stack.pop()
    }
}

export class CallOperation implements Operation {
    readonly procedureName: string

    constructor(procedureName: string) {
        this.procedureName = procedureName
    }

    run(stack: StackValue[]): void { //TODO fix so names don't have to be hardcoded
        if (this.procedureName == "add") {
            procedures.add(stack)
        } else if (this.procedureName == "sub") {
            procedures.sub(stack)
        } else if (this.procedureName == "mul") {
            procedures.mul(stack)
        } else if (this.procedureName == "div") {
            procedures.div(stack)
        }else {
            throw new Error(`${this.procedureName} not supported.`)
        }
    }
}

export class DupeOperation implements Operation {
    run(stack: StackValue[]): void {
        let last = stack[stack.length-1]
        stack.push(last)
    }
}

export class RollOperation implements Operation {
    run(stack: StackValue[]): void {
        let last = stack.pop() as StackValue //TODO check
        stack.unshift(last)
    }
}

export class SwapOperation implements Operation {
    run(stack: StackValue[]): void {
        let first = stack.pop() as StackValue //TODO check
        let second = stack.pop() as StackValue //TODO check
        stack.push(first, second)
    }
}
