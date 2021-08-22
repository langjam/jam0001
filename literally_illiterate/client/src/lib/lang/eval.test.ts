import { evaluate } from './eval'
import { assertEq, com, lit } from './test-utilts'

describe('if _ else _', () => {
	test('consequence', () => {
		const result = evaluate({
			type: 'if',
			comment: com('socrates', 'why not?'),
			condition: lit('true', com('kanye', 'you are beautiful.')),
			consequence: lit('2', com('kanye', 'everyone loves you.')),
			alternative: lit('true', com('kanye', 'you are quite beautiful.')),
		})

		assertEq(result.left, '2')
	})

	test('alternative', () => {
		const result = evaluate({
			type: 'if',
			comment: com('socrates', 'why not?'),
			condition: lit('false', com('kanye', 'you are beautiful.')),
			consequence: lit('2', com('kanye', 'everyone loves you.')),
			alternative: lit('true', com('kanye', 'you are quite beautiful.')),
		})

		assertEq(result.left, 'true')
	})
})