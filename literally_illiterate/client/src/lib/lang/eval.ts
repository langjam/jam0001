import { Either, Left, Right } from '$lib/trust';
import type { ExpressionAST } from './types';

type EvaluationError = {
	type: 'infix operator mismatch'
	left: string
	right: string
} | {
	type: 'not truthy'
	value: string
} | {
	type: 'not a number'
	value: string
} | {
	type: 'unimplemented'
	astType: string
} | {
	type: 'unreachable'
}

type EvaluationResult = Either<string, EvaluationError>

export const evaluate = (ast: ExpressionAST): EvaluationResult => {
	switch (ast.type) {
		case 'if':
			return evalIf(ast)
		case 'comparison':
			return evalCompare(ast)
		case 'math':
			return evalMath(ast)
		case 'literal':
			return Left(ast.value)
		default: {
			// `ast.type is never`
			// eslint-disable-next-line @typescript-eslint/ban-ts-comment
			// @ts-ignore
			const astType = ast.type
			return Right({ type: 'unimplemented', astType })
		}
	}
}

const evalIf = (ast: ExpressionAST & { type: 'if' }): EvaluationResult => {
	const cond = evalTruthy(ast.condition)
	if (cond.isRight) return Right(cond.right)

	if (cond.left) 
		return evaluate(ast.consequence)
	else 
		return evaluate(ast.alternative)
}

const evalMath = (ast: ExpressionAST & { type: 'math' }): EvaluationResult => {
	const left = evalNumber(ast.left)
	if (left.isRight) return Right(left.right)

	const right = evalNumber(ast.right)
	if (right.isRight) return Right(right.right)
	
	switch (ast.operator) {
		case '+': return Left(`${left.left + right.left}`)
		case '-': return Left(`${left.left - right.left}`)
		case '/': return Left(`${left.left / right.left}`)
		case '*': return Left(`${left.left * right.left}`)
	}
}

const evalCompare = (ast: ExpressionAST & { type: 'comparison' }): EvaluationResult => {
	switch (ast.operator) {
		case '<': case '>': case '>=': case '<=': {
			const left = evalNumber(ast.left)
			if (left.isRight) return Right(left.right)

			const right = evalNumber(ast.right)
			if (right.isRight) return Right(right.right)

			switch (ast.operator) {
				case '<':  return Left(`${left.left  < right.left}`)
				case '>':  return Left(`${left.left  > right.left}`)
				case '>=': return Left(`${left.left >= right.left}`)
				case '<=': return Left(`${left.left <= right.left}`)
			}

			break
		}
		// FIXME: using Either is making this pretty unreadable
		case 'and': case 'or': {
			const left = evalTruthy(ast.left)
			if (left.isRight) return Right(left.right)
			
			const right = evalTruthy(ast.right)
			if (right.isRight) return Right(right.right)
		
			if (ast.operator === 'or') 
				return Left(`${left.left || right.left}`)
			else 
				return Left(`${left.left && right.left}`)
		}
	}
}

const evalTruthy = (ast: ExpressionAST): Either<boolean, EvaluationError> => {
	const result = evaluate(ast)
	if (result.isRight) return Right(result.right)

	if (result.left === 'true') return Left(true)
	if (result.left === 'false') return Left(false)

	return Right({ type: 'not truthy', value: result.left })
}

const evalNumber = (ast: ExpressionAST): Either<number, EvaluationError> => {
	const result = evaluate(ast)
	if (result.isRight) return Right(result.right)
	
	try {
		return Left(parseFloat(result.left))
	} catch (e) {
		return Right({ type: 'not a number', value: result.left })
	}
}