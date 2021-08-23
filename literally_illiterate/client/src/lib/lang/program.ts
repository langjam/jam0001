import type { Either } from '$lib/trust'
import { choice } from '$lib/utils'
import { evaluate } from './eval'
import parse from './parse'
import type { RespondError } from './respond'
import { respond } from './respond'
import type { ProgramCtx, Sentiment } from './types'

interface ProgramPatches {
	sentiments?: Either<Sentiment, RespondError>[]
}

export const runProgram = async (
	ctx: ProgramCtx, input: string, patch?: ProgramPatches
): Promise<string> => {
	if (input.startsWith('man')) 
		return handleManual(input)

	const [parseResult, rest] = parse(input)
	if (parseResult.error) 			
		return handleParseFailure(parseResult.error, input, rest)

	const responseError = await respond(ctx, parseResult.unwrap, patch?.sentiments)
	if (responseError.isSome) 
		return handlePoorResponse(responseError.unwrap)
	
	const evaluation = evaluate(parseResult.unwrap)
	if (evaluation.isRight) {
		console.error(evaluation.right)
		return `Evaluation failed : ${evaluation.right.type}`
	}

	return evaluation.left
}

const handleManual = (input: string): string => {
	const tokens = input.split(' ')
	switch (tokens ? tokens[1] : '') { 
		case 'ownership': 
			return `kanye is literal (3 'text' true false)
				linus operates (+ - / *)
				tina compares (and or > < <= >=)
				socrates decides (if else)
				`
		case 'syntax':
			return '<name>, <comment>, "<code>"'
		case 'likes':
			return `kanye likes compliments
				linus likes to read hackernews headlines
				socrates likes questions
				tina likes a joke with a punchline`
		default:
			return `unknown manual: ${input}
			try: ownership, likes, or syntax.
			`
	}
}

const handleParseFailure = (e: Error, input: string, rest: string) => {
	// TODO do a diff on rest to get the position on input that failed
	return 'parse failure: ' + e.message
}

let apiFails = 0

const handlePoorResponse = (e: RespondError): string => {
	const apiFailMsg = apiFails > 1
		? '\nIf this error keeps occurring, you can set the inference engine to always accept your comments type:'
			+ '\nset-engine=always-accept'
		: ''

	switch (e.type) {
		//////////////////////// CITIZEN RESPONSES
		case 'recipient does not respond well':
			return `${e.comment.to} did not like ${e.sentiment}.`
		case 'recipient is does not own':
			return `${e.comment.to} does not own construct "${e.construct}"`
		case 'client is unaware of sentiment': {
			return `${e.comment.to} ` + choice([
				'shrugged you off',
				'ignored your comment',
				'looked confused by your comment',
			])
		}
		case 'recipient is asleep':
			return `${e.to} is asleep.`

		//////////////////////// API FAILURE
		case 'api: unknown error': {
			apiFails++
			return 'The sentiment inference engine had an unknown error. Try again soon.' + apiFailMsg
		}
		case 'network error': {
			apiFails++
			return 'Network error. Try again soon.' + apiFailMsg
		}
		case 'inference engine is loading': {
			apiFails++
			return `inference engine is loading. may take up to ${e.timeLeft} seconds.` + apiFailMsg
		}		

		default:
			// 'type does not exist on type never'
			// eslint-disable-next-line @typescript-eslint/ban-ts-comment
			// @ts-ignore
			return `program error: unhandled case ${e.type}`
	}
}