import { format } from './core/fmt'

export const println = (str: string, ...args: unknown[]): void => {
	console.log(format(str, ...args))
}