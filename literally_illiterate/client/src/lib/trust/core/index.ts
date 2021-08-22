import { implInto } from './into'
import { Option } from './option'
import { Result } from './result'

export * from './either'
export * from './fmt'
export { struct } from './globals'
export * from './into'
export * from './option'
export * from './primitive'
export * from './result'

implInto(Result, <T>(o: Option<T>): Result<T, Error> => {
	if (o.isNone) {
		return Result.Err(new Error('Conversion from Option resulted in None'))
	}

	return Result.Ok(o.unwrap)
})(Option)

implInto(Option, <T>(self: Result<T, Error>): Option<T> => {
	if (self.error) {
		return Option.None
	}

	return Option.Some(self.unwrap)
})(Result)