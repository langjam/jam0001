import { struct } from './globals'
import { ExpectError } from './option'

@struct('Result')
export class Result<T, E extends Error> {
	private _ok: T | null
	private _error: E | null

	constructor({ ok, error }: { ok?: T, error?: E }) {
		if (error != null && ok != null) {
			throw Error(`Cannot set none and some in result. \`error\` is '${error}' and \`ok\` is '${ok}'`)
		}

		if (error) {
			this._error = error
			this._ok = null
		} else if (ok != null) {
			this._ok = ok
			this._error = null
		} else {
			throw Error(`Expected 'ok' to not be null :: ${this._error}..${this._ok}`)
		}
	}

	get error(): E | null { return this._error }

	get unwrap(): T { 
		return this.expect(`Unwrapping result that has error "${this._error}"`)
	}
	
	expect(throwWithMessage: string): T { 
		if (this._error) 
			throw new ExpectError(throwWithMessage)
		
		return this._ok
	}

	static Ok<X>(value: X): Result<X, never> {
		return new Result<X, never>({ ok: value })
	}

	static Err<X extends Error>(value: X): Result<never, X> {
		return new Result({ error: value })
	}
}

// Useful for specifying return types that will be either T or Javascript Error
export type Res<T> = Result<T, Error>

// TODO would be useful if T could be optional and if not provided, default to Error
export type Err<T extends Error> = Result<never, T>

// Utility to make things look more Rusty
export const Err = (message: string | Error): Result<never, Error> => {
	if (message instanceof Error) {
		return Result.Err(message)
	}

	return Result.Err(new Error(message))
}

export const Ok = <T>(value: T): Result<T, never> => 
	Result.Ok(value)