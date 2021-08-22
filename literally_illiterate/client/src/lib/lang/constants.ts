import type { Citizen, ExpressionAST } from './types'
import type { WorldState } from './world'


// copy pasted from /data/acquisition/get-and-format-data
export const labelMap = { compliment: 0, question: 1, joke: 2, hacker: 3 }

export const citizens: Citizen[] = ['linus', 'socrates', 'tina', 'kanye']

export const firstClassCitizens: Record<keyof typeof labelMap, Citizen> = {
	hacker: 'linus',
	question: 'socrates',
	joke: 'tina',
	compliment: 'kanye',
}

export const firstClassHandler: Record<Citizen, ExpressionAST['type']> = {
	linus: 'math',
	socrates: 'if',
	tina: 'comparison',
	kanye: 'literal',
}

const DefaultState: { type: 'idle' } = { type: 'idle' }

export const DefaultWorld: WorldState = {
	totalEvents: 0,
	citizens: {
		linus: DefaultState,
		socrates: DefaultState,
		tina: DefaultState,
		kanye: DefaultState,
	}
}