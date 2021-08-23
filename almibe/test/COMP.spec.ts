/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { expect } from 'chai';
import { readdirSync, readFileSync } from 'fs';
import { COMPError, COMPInterpreter } from '../lib';
import { debug } from '../lib/debug';

//add files names below when you only want to run a set number of tests, leave empty to test all
const runOnly: Array<string> = [];

describe('COMP interpreter tests', () => {
    const interpreter = new COMPInterpreter();    
    readdirSync(__dirname + "/resources").forEach(dir => {
        readdirSync(__dirname + "/resources/" + dir).forEach(testFile => {
            if (testFile.endsWith("COMP") && (runOnly.length == 0 || runOnly.indexOf(testFile) != -1)) {
                it(testFile, () => {
                    const script = readFileSync(__dirname + "/resources/" + dir + "/" + testFile);
                    const expected = JSON.parse(readFileSync(__dirname + "/resources/" + dir + "/" + testFile.replace(/\.COMP$/, ".result")).toString());
                    const result = interpreter.run(script.toString());
                    if (testFile.includes('err')) {
                        expect(result instanceof COMPError).to.be.true;
                    } else {
                        if (result instanceof COMPError) {
                            throw result
                        } else {
                            expect(result).to.be.eql(expected);
                        }
                    }
                })
            }
        });
    });
});
