import { groupBy, highToLow, last, lowToHigh, range } from './common'
import { assertEq } from './testing'

describe('last', () => {
	assertEq(last([1]), 1)
	assertEq(last([1, 2, 3]), 3)
	assertEq(last([]), undefined)
	assertEq(last(undefined), undefined)
})

describe('low to high', () => {
	const arr = [2, 5, 1, 3, 4].map(x => ({ x }))
	const expected = [1, 2, 3, 4, 5].map(x => ({ x }))
	assertEq(arr.sort(lowToHigh('x')), expected)
})

describe('high to low', () => {
	const arr = [2, 5, 1, 3, 4].map(x => ({ x }))
	const expected = [5, 4, 3, 2, 1].map(x => ({ x }))
	assertEq(arr.sort(highToLow(d => d.x)), expected)
})

describe('range', () => {
	it('10: 0 - 9', () => {
		assertEq(range(10), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
	})
	
	it('10 from 5', () => {
		assertEq(range(10, 5), [5, 6, 7, 8, 9, 10, 11, 12, 13, 14])
	})
	
	it('10 step 2', () => {
		assertEq(range(10, 0, 2), [0, 2, 4, 6, 8, 10, 12, 14, 16, 18])
	})
})

describe('groupBy', () => {
	it('even odd', () => {
		assertEq(
			groupBy([1, 2, 3, 4, 5, 6, 7, 8, 9], v => (v % 2 ? 'odd' : 'even')),
			{ odd: [1, 3, 5, 7, 9], even: [2, 4, 6, 8] }
		)
	})
	
	const colors = [
		'Apricot',
		'Brown',
		'Burgundy',
		'Cerulean',
		'Peach',
		'Pear',
		'Red',
	]

	it('group by colors name first letter', () => {
		assertEq(groupBy(colors, v => v[0]), {
			A: ['Apricot'],
			B: ['Brown', 'Burgundy'],
			C: ['Cerulean'],
			P: ['Peach', 'Pear'],
			R: ['Red'],
		})
	})
	
	it('group by length of color names', () => {
		assertEq(groupBy(colors, v => v.length), {
			3: ['Red'],
			4: ['Pear'],
			5: ['Brown', 'Peach'],
			7: ['Apricot'],
			8: ['Burgundy', 'Cerulean'],
		})
	})

	const data = [
		{ comment: 'abc', forItem: 1, inModule: 1 },
		{ comment: 'pqr', forItem: 1, inModule: 1 },
		{ comment: 'klm', forItem: 1, inModule: 2 },
		{ comment: 'xyz', forItem: 1, inModule: 2 },
	];

	it('group by module', () => {			
		assertEq(groupBy(data, v => v.inModule), {
			1: [
				{ comment: 'abc', forItem: 1, inModule: 1 },
				{ comment: 'pqr', forItem: 1, inModule: 1 },
			],
			2: [
				{ comment: 'klm', forItem: 1, inModule: 2 },
				{ comment: 'xyz', forItem: 1, inModule: 2 },
			],
		})
	})

	it('group by module with item', () => {			
		assertEq(groupBy(data, x => x.forItem + '-' + x.inModule), {
			'1-1': [
				{ comment: 'abc', forItem: 1, inModule: 1 },
				{ comment: 'pqr', forItem: 1, inModule: 1 },
			],
			'1-2': [
				{ comment: 'klm', forItem: 1, inModule: 2 },
				{ comment: 'xyz', forItem: 1, inModule: 2 },
			],
		})

	})
})