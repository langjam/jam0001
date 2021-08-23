import { assertEq } from '$lib/utils/testing';
import { Int } from './primitive';

it('parses to Int', () => {
	const x = Int.parse(23) 
	assertEq(x.unwrap.get, 23)
	const y = Int.parse(-23) 
	assertEq(y.unwrap.get, -23)
})

it('parses to Error', () => {
	const x = Int.parse(23.2)
	assertEq(x.error.message, '23.2 is not a valid integer')
	const y = Int.parse(null)
	assertEq(y.error.message, 'null is not a valid integer')
})