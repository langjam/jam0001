import { format } from '$lib/trust';
import assert from 'assert';
// import { DateTime } from 'luxon';

// struct('DateTime')(DateTime)
// implFmt((t: DateTime) => t.toISO())(DateTime)

export const removeTabsNewLines = (str: string): string => {
	return str.split('\t').join('').split('\n').join('')
}

export const assertEq = <T>(actual: T, expected: T, message?: string): void => {
	try {
		assert.deepStrictEqual(actual, expected, message)
	} catch(e) {
		if (e instanceof assert.AssertionError) {
			throw new assert.AssertionError({
				...e,
				actual: removeTabsNewLines(format('{}', actual)),
				expected: removeTabsNewLines(format('{}', expected)),
			})
		}

		throw e
	}
}