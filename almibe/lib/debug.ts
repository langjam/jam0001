/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

export function debug(message: string, a: any): any {
    console.log(message + ": " + JSON.stringify(a, (key, value) => typeof value === "bigint" ? value.toString() + "n" : value, 4));
    return null
}

export function TODO(message: string = "Not implemented."): any {
    throw new Error(`TODO: ${message}`);
}
