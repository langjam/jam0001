import { Left, Right } from '$lib/trust/core/either'
import { DefaultWorld } from './constants'
import { respond } from './respond'
import { assertEq, com, lit } from './test-utilts'

describe('if _ else _', () => {
	const defaultCtx = {
		inferenceEndpoint: '',
		world: DefaultWorld,
	}

	test('a okay', async () => {
		const result = await respond(defaultCtx, {
			type: 'if',
			comment: com('socrates', 'why not?'),
			condition: lit('true', com('kanye', 'you are beautiful.')),
			consequence: lit('2', com('kanye', 'everyone loves you.')),
			alternative: lit('true', com('kanye', 'you are quite beautiful.')),
		}, [
			Left('question'),
			Left('compliment'),
			Left('compliment'),
			Left('compliment'),
		])

		if (result.isSome) console.error(result.unwrap)

		assertEq(result.isNone, true)
	})

	test('network failure', async () => {
		const c1 = com('kanye', 'you are beautiful.')
		const c2 = com('kanye', 'everyone loves you.')

		const result = await respond(defaultCtx, {
			type: 'if',
			comment: com('socrates', 'why not?'),
			condition: lit('true', c1),
			consequence: lit('2', c2),
			alternative: lit('true', com('kanye', 'you are quite beautiful.')),
		}, [
			Left('question'),
			Right({ type: 'network error', comment: c1, sentiment: 'NONE' }),
			// shouldn't get reached
			Right({ type: 'network error', comment: c2, sentiment: 'NONE' }),
			Left('compliment'),
		])

		assertEq(result.unwrap, { type: 'network error', comment: c1, sentiment: 'NONE' })
	})
})