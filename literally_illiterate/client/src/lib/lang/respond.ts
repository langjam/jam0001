import { Either, Left, Option, Right } from '$lib/trust';
import { firstClassCitizens, firstClassHandler, labelMap } from './constants';
import type { Citizen, Comment, ExpressionAST, ProgramCtx, Sentiment } from './types';
import type { WorldState } from './world';

export type RespondError = ({
	type: 'network error' 
	comment: Comment
} | {
	type: 'api: unknown error'
		| 'client is unaware of sentiment'
		| 'recipient does not respond well'
	sentiment: string
	comment: Comment
} | {
	type: 'inference engine is loading'
	comment: Comment
	timeLeft: string
} | {
	type: 'recipient is asleep'
	to: Citizen
} | {
	type: 'recipient is does not own'
	comment: Comment
	construct: ExpressionAST['type']
})

// walks the ast to see how citizens respond to comments
// if they all respond okay, return None (yeah thats confusing, should fix)
// else, return reason 
export const respond = async (
	ctx: ProgramCtx, ast: ExpressionAST, testSentiments?: Either<Sentiment, RespondError>[]
): Promise<Option<RespondError>> => {

	const astQueue = flattenAST(ast)
	
	if (testSentiments) {
		if (testSentiments.length !== astQueue.length) 
			throw Error(`respond with test emotes :: ${testSentiments.length} !== ${astQueue.length}`)
	}

	for (let i = 0; i < astQueue.length; i++) {
		const { comment, type } = astQueue[i]

		if (firstClassHandler[comment.to] !== type)
			return Option.Some({
				type: 'recipient is does not own',
				comment,
				construct: type,
			})

		if (ctx.inferenceEngine === 'always-accept')
			break

		const sentiment = testSentiments 
			// ugly way to monkey patch for testing (see emote.test.ts)
			? testSentiments[i]
			: (await getSentiment(ctx.inferenceEndpoint, comment))
		
		if (sentiment.isRight) 
			return Option.Some(sentiment.right)

		const response = recipientResponse(ctx.world, sentiment.left, comment.to)
		
		if (response.isRight)
			return Option.Some(response.right)

		const respondedWell = response.left

		if (respondedWell == false) 
			return Option.Some({ 
				type: 'recipient does not respond well', 
				comment, 
				sentiment: sentiment.left,
			})
	}

	return Option.None
}

// calls api to get the label for comment
// fixme: sentiment may not be the best word to describe this
const getSentiment = async (inferenceEndpoint: string, comment: Comment): Promise<Either<Sentiment, RespondError>> => {
	let res: Response

	try {
		res = await fetch(inferenceEndpoint, { 
			method: 'post',
			body: JSON.stringify({ comment: comment.comment }),
		})
	} catch (e) {
		console.error(e)
		return Right({ type: 'network error', comment })
	}

	const sentiment = await res.text()

	console.log({ sentiment })

	if (res.status !== 200) {
		if (sentiment.includes('HuggingFace.loading.')) {
			const timeLeft = sentiment.split('HuggingFace.loading.')[1]
			return Right({ type: 'inference engine is loading', comment, timeLeft })	
		}

		console.error({ comment, text: sentiment })
		return Right({ type: 'api: unknown error', comment, sentiment })
	}

	if (labelMap[sentiment] != null) 
		// eslint-disable-next-line @typescript-eslint/ban-ts-comment
		// @ts-ignore
		return Left(sentiment)

	console.error('Server replied with unknown sentiment', sentiment)

	return Right({ type: 'client is unaware of sentiment', comment, sentiment })
}

const flattenAST = (ast: ExpressionAST): ExpressionAST[] => {
	switch (ast.type) {
		case 'if':
			return [
				ast,
				...flattenAST(ast.condition), 
				...flattenAST(ast.consequence), 
				...flattenAST(ast.alternative),
			]
		case 'literal':
			return [ast]
		case 'math':
		case 'comparison':
			return [
				ast, 
				...flattenAST(ast.left),
				...flattenAST(ast.right),
			]
		default: {
			// `ast.type is never`
			// eslint-disable-next-line @typescript-eslint/ban-ts-comment
			// @ts-ignore
			const type = ast.type
			throw new Error(`get comments: unimplemented :: ${type}`)
		}
	}
}

const recipientResponse = (world: WorldState, sentiment: Sentiment, to: Citizen): Either<boolean, RespondError> => {
	const state = world.citizens[to]

	if (state.type === 'sleeping')
		return Right({ type: 'recipient is asleep', to, sentiment })

	return Left(firstClassCitizens[sentiment] === to)
}