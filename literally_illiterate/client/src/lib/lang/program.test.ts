import { Left } from '$lib/trust';
import { DefaultWorld } from './constants';
import { runProgram } from './program';
import { assertEq } from './test-utilts';
import type { MathOperator, ProgramCtx } from './types';
import { updateCitizen } from './world';

const ctx: ProgramCtx = { world: DefaultWorld, inferenceEndpoint: '', inferenceEngine: 'language-model' }

test('ownership', async () => {
	const result = await runProgram(ctx,
		'tina, setup? punchline! "3"', 		
		{
			sentiments: [Left('joke')]
		}
	)

	assertEq(result, 'tina does not own construct "literal"')
})

describe('if else', () => {

	test('simple', async () => {
		const result = await runProgram(ctx,
			`
			socrates, why not? "if else"
				| kanye, you are beautiful. "false"
				| kanye, everyone loves you. "2"    
				| kanye, you are quite beautiful. "true"
			`, 		
			{
				sentiments: [
					Left('question'),
					Left('compliment'),
					Left('compliment'),
					Left('compliment'),
				]
			}
		)
	
		assertEq(result, 'true')
	})
	
	test('invalid syntax - expects consequence', async () => {
		const result = await runProgram(ctx,
			`
			socrates, why not? "if else"
				| tina, setup? punchline! "and"
					| kanye, everyone loves you. "false"
					| kanye, you are quite beautiful. "true"
			`, 		
			{
				sentiments: [
					Left('joke'),
				]
			}
		)
	
		assertEq(result, "parse failure: Expected 'consequence'")
	})
	
	test('nesting', async () => {
		const result = await runProgram(ctx,
			`
			socrates, why not? "if else"
				| tina, some joke. "<="
					| kanye, compliment a. "100"    
					| kanye, compliment b. "200"
				| kanye, compliment c. "2"    
				| kanye, compliment d. "true"
			`, 
			{
				sentiments: [
					Left('question'),
					Left('joke'),
					Left('compliment'), // a
					Left('compliment'), // b
					Left('compliment'), // c
					Left('compliment'), // d
				]
			}
		)
	
		assertEq(result, '2')
	})	
	
	test('nesting a lot', async () => {
		const result = await runProgram(ctx,
			`
			socrates, why not? "if else"
				| tina, setup? punchline! "and"
					| kanye, a. "false"
					| kanye, b. "true"
				| linus, hackernews "*"
					| kanye, c. "3"
					| kanye, d. "2"
				| tina, setup? punchline! "<"
					| linus, hackernews "*"
						| kanye, e. "5"
						| linus, hackernews "/"
							| kanye, f. "10"
							| kanye, g. "2"
					| kanye, h. "26"
			`, 
			{
				sentiments: [
					Left('question'),
					Left('joke'),
					Left('compliment'), // a
					Left('compliment'), // b
					Left('hacker'),
					Left('compliment'), // c
					Left('compliment'), // d
					Left('joke'),
					Left('hacker'),
					Left('compliment'), // e
					Left('hacker'),
					Left('compliment'), // f
					Left('compliment'), // g
					Left('compliment'), // h
				]
			}
		)
	
		assertEq(result, 'true')
	})	
})

describe('world', () => {
	test('sleeping basic', async () => {
		const world = updateCitizen(DefaultWorld, 'kanye', 'sleeping')

		const result = await runProgram({ ...ctx, world },
			'kanye, compliment a. "100"', 
			{ sentiments: [Left('compliment')] },
		)
	
		assertEq(result, 'kanye is asleep.')
	})
})

describe('math', () => {
	const ops: MathOperator[] = ['+', '-', '/', '*']

	test('3, 4', async () => {
		const expected = ['7', '-1', '0.75', '12']

		for (let i = 0; i < ops.length; i++) {
			const result = await runProgram(ctx ,
				`linus, insert hn title "${ops[i]}" 
					| kanye, everyone loves you. "3" 
					| kanye, you are quite beautiful. "4"
				`,
				{ 
					sentiments: [
						Left('hacker'),
						Left('compliment'),
						Left('compliment'),
					] 
				},
			)
			
			assertEq(result, expected[i], ops[i])
		}
	})
})

// TODO
/**
Now:
plane on change do update for 1sec
mobile style
[maybe] in "<code>" you can write a comment that wakes someone up half the time
collect more data

go through TODOs
 */

/*
kanye: thank you _$eval_

question
can i get that book?
do you have any drink coupons?
will you die?
where are you headed?
are you single?
did you want an extra pillow?

TODO maybe prune questions from insults
*/ 