import { implFmt } from './fmt'
import { struct } from './globals'
import { Err, Result } from './result'
import { range } from './utils'

@implFmt((int: Int) => `${int.get}`)
@struct('Int')
export class Int {
	private _value: number

	get get(): number { return this._value }	

	constructor(x: number) {
		if (!Int.is(x)) {
			throw new Error(`Expected int, got ${x}`)
		}

		this._value = x
	}

	static is(x: number): boolean { return x != null && x.toFixed(0) === `${x}` }	

	static fromFloat(x: number): Int { return new Int(Math.floor(x)) }

	static parse(x: number | string): Result<Int, Error> {
		const num: number = (() => {
			switch(typeof x) { 
				case 'number': return x 
				// The `Int.is` check will ensure that this is an int, despite us parsing as a float
				case 'string': return parseFloat(x)
				default: return x
			}
		})()

		if (Int.is(num)) {
			return Result.Ok(new Int(num))
		}

		return Err(`${num} is not a valid integer`)
	}

	range(): number[] {
		return range(this._value)
	}

    eq(x: Int): boolean { return this.get === x.get }
    neq(x: Int): boolean { return this.get !== x.get }

    add(x: Int): Int { return new Int(this.get + x.get) }
    sub(x: Int): Int { return new Int(this.get - x.get) }
    mul(x: Int): Int { return new Int(this.get * x.get) }
    div(x: Int): Int { return Int.fromFloat(this.get / x.get) }

	static sum<T>(x: T[], getValue: (t: T) => Int): Int {
		const result = x.reduce((acc, i) => acc + getValue(i).get, 0)
		return new Int(result)
	}
}