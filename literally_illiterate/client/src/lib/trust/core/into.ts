import type { ConstructorOf } from './globals'
import { getStructName, GLOBALS, ImplStructNotFoundError } from './globals'
import { Err, Result } from './result'

export function implInto<
	INSTANTIATED, 
	TARGET
>(target: ConstructorOf<TARGET>, into: (t: INSTANTIATED) => TARGET) {
	return (constructor: ConstructorOf<INSTANTIATED>): void => {
		if (!getStructName(constructor)) 
			throw new ImplStructNotFoundError(constructor, 'implInto')

		if (!getStructName(target)) 
			throw new ImplStructNotFoundError(target, 'implInto')

		if (!GLOBALS.impl.into[getStructName(constructor)]) {
			GLOBALS.impl.into[getStructName(constructor)] = {}
		}

		GLOBALS.impl.into[getStructName(constructor)][getStructName(target)] = into
	}
}

export function into<SOURCE, TARGET>(target: ConstructorOf<TARGET>, source: SOURCE): Result<TARGET, Error> {
	if (!getStructName(source)) return Err(new ImplStructNotFoundError(source, 'implInto'))
	if (!getStructName(target)) return Err(new ImplStructNotFoundError(target, 'implInto'))

	const intoFns = GLOBALS.impl.into[getStructName(source)]

	if (!intoFns || !intoFns[getStructName(target)])
		return Err(`into was not implemented for ${getStructName(source)} to ${getStructName(target)} (implInto)`)

	// eslint-disable-next-line @typescript-eslint/ban-ts-comment
	// @ts-ignore
	return Result.Ok(intoFns[getStructName(target)](source))
}