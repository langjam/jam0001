export * from './core';
export * from './std';

import { Option } from './core';

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const deepEqual = function (x: any, y: any) {
	if (x === y) {
		return true;
	}
	else if ((typeof x == 'object' && x != null) && (typeof y == 'object' && y != null)) {
		if (Object.keys(x).length != Object.keys(y).length)
			return false;
  
		for (const prop in x) {
			// eslint-disable-next-line no-prototype-builtins
			if (y.hasOwnProperty(prop)) {  
				if (! deepEqual(x[prop], y[prop]))
					return false;
			}
			else
				return false;
		}

		return true;
	}
	else 
		return false;
}

export const match = <T, R>(x: T, ...arms: [T, R][]): Option<R> => {
	const value = arms.find(([y, returnValue]) => {
		if (deepEqual(x, y)) return returnValue
	})

	return value && value.length === 2 && value[1] ? Option.Some(value[1]) : Option.None
}	