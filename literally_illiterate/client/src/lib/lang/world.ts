import { choice } from '$lib/utils'
import { citizens } from './constants'
import type { Citizen } from './types'

type CitizenState = {
	type: 'idle' | 'sleeping'
}

export interface WorldState {
	totalEvents: number
	citizens: Record<Citizen, CitizenState>
}

// if null, no change occurred
type UpdateSet = [WorldState, string | null] | null

const snooze = (w: WorldState, c: Citizen): UpdateSet => {
	if (w.citizens[c].type === 'sleeping') return null
	return [updateCitizen(w, c, 'sleeping'), `${c} went to sleep.`]
}

const awake = (w: WorldState, c: Citizen): UpdateSet => {
	if (w.citizens[c].type !== 'sleeping') return null
	return [updateCitizen(w, c, 'idle'), `${c} woke up`]
}

const citizensWhoAre = (type: CitizenState['type'], w: WorldState): Citizen[] => 
	Object.entries(w.citizens)
		.filter(x => x[1].type === type)
		.map<Citizen>((x: [Citizen, CitizenState]) => x[0])

export const updateCitizen = (w: WorldState, c: Citizen, type: CitizenState['type']): WorldState => {
	return {
		...w,
		citizens: { ...w.citizens, [c]: { type } }
	}
}

export const updateWorld = (_world: WorldState): [WorldState, string[]] => {
	let world = { ..._world, totalEvents: _world.totalEvents + 1 }
	const changes = []

	const update = (update?: [WorldState, string]) => {
		if (!update) return
		world = update[0]
		changes.push(update[1])
	}

	if (world.totalEvents % 5 == 0) 
		update(snooze(world, choice(citizens)))	

	if (world.totalEvents % 3 == 0) 
		update(awake(world, choice(citizens)))	

	console.count('World events')
	
	return [world, changes]
}