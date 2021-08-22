import type { labelMap } from './constants'
import type { WorldState } from './world'

export type Sentiment = keyof typeof labelMap

export type Emotion = ({
	type: 'ok' | 'err',	
} & ({
	type: 'ok',
	evaluation: string
} | {
	type: 'err',
	reason: string,
}))

export type Citizen = 'linus' | 'socrates' | 'tina' | 'kanye'

export type ProgramCtx = {
	world: WorldState
	inferenceEngine: 'language-model' | 'always-accept'
	inferenceEndpoint: string
}

export type ProgramState = { 
	world: WorldState,
	working: boolean, 
	evaluation: string, 
	history: { input: string, output: string }[],
}

// TODO remove js payload
export type Comment = { to: Citizen, comment: string, js_payload?: string }

export type MathOperator = '+' | '-' | '/' | '*'
export type ComparisonOperator = '<' | '>' | '<=' | '>=' | 'or' | 'and'

export type ExpressionAST = {
	type: 'if' | 'literal' | 'comparison' | 'math'
	comment: Comment
} & ({
	type: 'if',
	condition: ExpressionAST,
	consequence: ExpressionAST,
	alternative: ExpressionAST
} | {
	// kanye
	type: 'literal'
	value: string
} | {
	// linus
	// infix only
	type: 'math'
	operator: MathOperator
	left: ExpressionAST
	right: ExpressionAST
} | {
	// tina fey
	type: 'comparison'	
	operator: ComparisonOperator
	left: ExpressionAST
	right: ExpressionAST
})