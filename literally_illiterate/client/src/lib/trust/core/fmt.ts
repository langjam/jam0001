import type { Constructor } from './globals'
import { getStructName, GLOBALS, ImplStructNotFoundError } from './globals'
import type { Res } from './result'
import { Err, Result } from './result'
import { range } from './utils'

export function implFmt<T extends Constructor, INSTANTIATED>(format: (t: INSTANTIATED) => string) {
	return (constructor: T): void => {
		if (!getStructName(constructor)) 
			throw new ImplStructNotFoundError(constructor, 'implInto')

		GLOBALS.impl.fmt[getStructName(constructor)] = format
	}
}

/**
 * Object/string interpolation to string
 * @example
	struct Abc { value: string }

	impl fmt::Display for Point {
		fmt(self): FmtRes {
			return Ok(self.value)
		}
	}

	let formatted = format("my abc {}", Abc { value: "123" })
	assert!(formatted, "my abc 123")
 */
export const format = (str: string, ...args: unknown[]): string => {
	const sections = str.split('{}')

	if (str !== '{}' && args.length !== 0 && args.length !== sections.length - 1) {
		throw Error(`println: Arg mismatch. Expected ${sections.length - 1} args. Got ${args.length}.`)
	}

	const tabs = (n: number) => range(n).map(_ => '\t').join('')

	const formatWithTabs = (a: unknown, level=0, startWithTabs=false): string => {
		if (typeof a === 'object') {
			if (a == null) return `${a}`

			const aFormatted = fmt(a)
			if (!aFormatted.error) return aFormatted.unwrap

			if (Array.isArray(a)) {
				const formatted = a.map(a => formatWithTabs(a, level + 1, true)).join('\n')
				return a.length === 0 ? '[]' : '[\n' + formatted + `\n${tabs(level)}]`
			}

			const formatted = Object.entries(a)
				.map(([key, value]) => 
					tabs(level + 1) + `${key}: ${formatWithTabs(value, level + 1)}`
				)
				.join('\n')

			return (startWithTabs ? tabs(level) : '') + '{\n' + formatted + '\n' + tabs(level) + '}'
		}

		return `${a}`
	}

	const argsFormatted = args.map(a => formatWithTabs(a, 0, false))
	
	return sections.reduce((acc, section, i) => {
		return acc + section + (argsFormatted[i] ?? '')
	}, '')
}

export type FmtRes = Res<string>

// eslint-disable-next-line @typescript-eslint/ban-types
export const fmt = (toFormat: object): FmtRes => {
	const instance = getStructName(toFormat)

	if (instance && GLOBALS.impl.fmt[instance]) 
		return Result.Ok(GLOBALS.impl.fmt[instance](toFormat))

	return Err('Implementation not found')
}