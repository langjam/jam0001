import { assertEq } from '$lib/utils/testing';
import { into } from './into';
import { Option } from './option';
import { Err, Result } from './result';

describe('into Option', () => {
	test('from Ok Result', () => {
		const res = Result.Ok('Yeet')
		const converted = into(Option, res).expect('implementation from Result into Option')
		
		assertEq(converted.unwrap, 'Yeet')
	})	
	
	test('from Error Result', () => {
		const res = Err('Not so yeet')
		const converted = into(Option, res).expect('implementation from Result into Option')
		
		assertEq(converted.isNone, true)
	})	
})

describe('into Result', () => {
	test('from None Option', () => {
		const none = Option.None
		const converted = into(Result, none).expect('implementation from Option into Result')
				
		assertEq(converted.error.message, 'Conversion from Option resulted in None')
	})	

	test('from Some Option', () => {
		const some = Option.Some('Hello')
		const converted = into(Result, some).expect('implementation from Option into Result')
		
		assertEq(converted.unwrap, 'Hello')
	})	
})