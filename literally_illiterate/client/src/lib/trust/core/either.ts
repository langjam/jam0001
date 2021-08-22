import { struct } from './globals'
import { Option } from './option'

@struct('Either')
export class Either<L, R> {
	private _left: Option<L>
	private _right: Option<R>

	constructor({ left, right }: { left?: L, right?: R }) {
		if (right != null && left != null) {
			throw Error(`Cannot set none and some in result. \`error\` is '${right}' and \`ok\` is '${left}'`)
		}

		if (right) {
			this._right = Option.Some(right)
			this._left = Option.None
		} else if (left != null) {
			this._left = Option.Some(left)
			this._right = Option.None
		} else {
			throw Error(`Expected 'left' to not be null :: ${this._right}..${this._left}`)
		}
	}

	get isRight(): boolean { return this._right.isSome }
	get isLeft(): boolean { return this._left.isSome }

	get left(): L { return this._left.expect('left') }
	get right(): R { return this._right.expect('right') }

	static Left<X>(value: X): Either<X, never> {
		return new Either<X, never>({ left: value })
	}

	static Right<X>(value: X): Either<never, X> {
		return new Either({ right: value })
	}
}

export const Left = <T>(value: T): Either<T, never> => 
	Either.Left(value)

export const Right = <T>(value: T): Either<never, T> => 
	Either.Right(value)