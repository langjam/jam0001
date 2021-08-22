export const choice = <T>(arr: T[]): T => 
	arr[Math.floor(Math.random() * arr.length)]

export const dedupe = <T>(array: T[]): T[] => {
	return [...new Set(array)]
}

export const lowToHigh = <T, Key extends keyof T>(key: Key | ((t: T) => number)) => (a: T, b: T): number => {
	const getSort = typeof key === 'function'
		? key
		: (x: T) => x[key]
	return (+getSort(a)) - (+getSort(b))
}

export const highToLow = <T, Key extends keyof T>(key: Key | ((t: T) => number)) => (a: T, b: T): number => {
	const getSort = typeof key === 'function'
		? key
		: (x: T) => x[key]
	return (+getSort(b)) - (+getSort(a))
}

export const last = <T>(x: T[] = []): T => x[x?.length - 1]

export const range = (N: number, from=0, step=1): number[] => 
	[...Array(N)].map((_, i) => from + i * step)

// https://stackoverflow.com/a/64489535/5770245
export const groupBy = <X, Key extends string | symbol | number>(x: X[], f: (x: X) => Key): Record<Key, X[]> => 
	// eslint-disable-next-line @typescript-eslint/ban-ts-comment
	// @ts-ignore
	x.reduce(
		(acc, b: X) => {
			const key = f(b)
			// eslint-disable-next-line @typescript-eslint/ban-ts-comment
			// @ts-ignore
			acc[key] = (acc[key] ?? [])
			// eslint-disable-next-line @typescript-eslint/ban-ts-comment
			// @ts-ignore
			acc[key].push(b)
			return acc
		}, 
		{}
	)

export function debounce(func: (..._) => void, timeout = 300): (..._) => void {
	let timer
	return (...args) => {
		clearTimeout(timer);
		timer = setTimeout(() => { func.apply(this, args); }, timeout);
	}
}