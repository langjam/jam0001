import type { Res } from '$lib/trust';
import { Err, ExpectError, Ok, Option } from '$lib/trust';
import { citizens } from './constants';
import type { Citizen, Comment, ComparisonOperator, ExpressionAST, MathOperator } from './types';

// expression ast, rest of input
export type ParseResult = [Res<ExpressionAST>, string]

type ParseExp<T> = (comment: Comment, expString: string, rest: string) => 
	[Res<ExpressionAST & { type: T }>, string]

const debug = (...args) => {
	const d = false
	if (d) console.debug(args)
}

// FIXME: Statement is a misnomer
const parseStatement = (input: string): Res<{ comment: Comment, toParse: string }> => {
	const matches = /^(\w+)\s*,\s*(.*)\s*"(.*)"$/g.exec(input.trim())

	if (matches?.length != 4) 
		return Err(`Bad input: ${input}.\n\n`
			+ 'Syntax: <citizen\'s name>, <your comment> "<code>"'
		)

	const [_, to, comment, toParse] = matches.map(x => x.trim())

	const isCitizen = (x: string): x is Citizen => 
		// eslint-disable-next-line @typescript-eslint/ban-ts-comment
		// @ts-ignore
		citizens.includes(x)

	if (isCitizen(to)) 
		return Ok({ toParse, comment: { to, comment }, })

	return Err(`${to}'s name is not recognized`)
}

const parse = (input: string): ParseResult => {
	// parse fns can throw because they use expect/unwrap
	try {
		return _parse(input)	
	} catch (e) {
		if (e instanceof ExpectError) 
			return [Err(e.message), '?']

		return [Err('Unexpected error: ' + e.message), '?']
	}
}

const _parse = (input: string): ParseResult => {
	const [first, ...restPieces] = input.replace(/(\t|\n)/g, '').split('|')
	const rest = restPieces.join(' | ')

	if (parseStatement(first).error)
		return [Err(parseStatement(first).error.message), rest]

	const { comment, toParse } = parseStatement(first).unwrap

	if (toParse === 'if else') 
		return parseIf(comment, toParse, rest)		
	else if (comparisonOperatorMatch(toParse).isSome) 
		return parseComparison(comment, toParse, rest)
	else if (mathOperatorMatch(toParse).isSome) 
		return parseMath(comment, toParse, rest)
	else if (literalMatch(toParse).isSome) {
		const value = literalMatch(toParse).expect('literal match')
		debug('literal match', value)
		return [Ok({ type: 'literal', value, comment }), rest]
	}

	return [Err(`${toParse} unrecognized`), rest]
}

// 1 '213' true false
const literalMatch = (input: string): Option<string> => {
	try {
		const [_, match] = /^(\d+|true|'.*'|false)$/g.exec(input.trim())
		return Option.Some(match)
	} catch (e) {
		return Option.None
	}
}

const comparisonOperatorMatch = (input: string): Option<ComparisonOperator> => {
	try {
		const [_, match] = /^(<|>|<=|>=|and|or)$/g.exec(input.trim())
		// eslint-disable-next-line @typescript-eslint/ban-ts-comment
		// @ts-ignore
		return Option.Some(match)
	} catch (e) {
		return Option.None
	}
}

const mathOperatorMatch = (input: string): Option<MathOperator> => {
	try {
		const [_, match] = /^(\+|\/|-|\*)$/g.exec(input.trim())
		// eslint-disable-next-line @typescript-eslint/ban-ts-comment
		// @ts-ignore
		return Option.Some(match)
	} catch (e) {
		return Option.None
	}
}

const parseMath: ParseExp<'math'> = (comment, operatorString, nestedInput) => {
	const operator = mathOperatorMatch(operatorString).expect('math operator')

	const [l, rest1] = _parse(nestedInput)
	const left = l.expect('left')

	const [r, rest2] = _parse(rest1)
	const right = r.expect('right')

	debug('comparison match', operator, left, right)

	return [Ok({ type: 'math', operator, comment, left, right }), rest2]
}

const parseComparison: ParseExp<'comparison'> = (comment, operatorString, nestedInput) => {
	const operator = comparisonOperatorMatch(operatorString).expect('comparison operator')

	const [l, rest1] = _parse(nestedInput)
	const left = l.expect('left')

	const [r, rest2] = _parse(rest1)
	const right = r.expect('right')

	debug('comparison match', operator, left, right)

	return [Ok({ type: 'comparison', operator, comment, left, right }), rest2]
}

/**
socrates, why not? "if else"
	| kanye, you are beautiful. "false"   < condition                (should eval to truthy value)
	| kanye, you are beautiful. "2"       < followed by consequence  (any exp)
	| kanye, you are beautiful. "false"   < followed by alternative  (any exp)
 */
const parseIf: ParseExp<'if'> = (comment, _, nestedInput) => {
	const [cond, rest1] = _parse(nestedInput)
	const condition = cond.expect('condition')
	const [cons, rest2] = _parse(rest1)
	const consequence = cons.expect('consequence')
	const [alt, rest] = _parse(rest2)
	const alternative = alt.expect('alternative')

	// console.debug('if _ else _', {condition, consequence, alternative})

	return [Ok({
		type: 'if',
		condition,
		consequence,
		alternative,
		comment,
	}), rest]
}

export default parse