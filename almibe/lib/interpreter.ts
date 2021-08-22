/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { TODO } from "./debug"
import { Operation } from "./operations"

export type StackValue = number

export function interpret(operations: Array<Operation>, stack: Array<StackValue> = new Array()): Array<StackValue> {
    for (let operation of operations) {
        operation.run(stack)
    }

    return stack
}
