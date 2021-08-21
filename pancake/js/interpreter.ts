
export interface StateInfo {
    currentLine: number;
    activeLines: number[];
}

export class Interpreter {
    lines: string[]

    constructor(lines: string[]) {
        this.lines = lines;
    }

    async step(): Promise<StateInfo> {
        const state: StateInfo = {
            currentLine: 0,
            activeLines: []
        };
        return state;
    }
}