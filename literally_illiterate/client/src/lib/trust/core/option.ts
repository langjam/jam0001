import { struct } from './globals'

export class ExpectError extends Error {
	constructor(message: string) {
		super('Expected \'' + message + '\'')
	}
}

@struct('Option')
export class Option<T> {
	private _some: T | null
	private _none: boolean

	constructor({ some, none }: { some?: T, none: boolean }) {
		if (none === true && some != null) {
			throw Error(`Cannot set none and some in option. \`none\` is 'true' and \`some\` is '${some}'`)
		}

		if (none) {
			this._none = none
			this._some = null
		} else if (some != null) {
			this._some = some
			this._none = false
		} else {
			throw Error(`Expected some to not be null :: ${this._none}..${this._some}`)
		}
	}

	get isSome(): boolean { return !this._none }

	get isNone(): boolean { return this._none }

	get unwrap(): T { 
		return this.expect('Unwrapping option that only has none')
	}
	
	unwrapOr<OR>(or: OR): T | OR { 
		if (this._none) 
			return or
		
		return this._some
	}
	
	expect(throwWithMessage: string): T { 
		if (this._none) 
			throw new ExpectError(throwWithMessage)
		
		return this._some
	}

	static Some<X>(value: X): Option<X> {
		return new Option<X>({ some: value, none: false })
	}

	static get None(): Option<never> {
		return new Option({ none: true })
	}
}