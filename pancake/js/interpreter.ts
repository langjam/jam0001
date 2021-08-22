export interface StateInfo {
    universe: string,
    currentLine: number;
    activeLines: number[];
    stack: Value[]
}

export type Value = 
    | string
    | number
    | Value[]
    | { type: 'Function' };

export type Result = SuccessfulResult | FailureResult;

interface SuccessfulResult {
    successful: true,
    commentLines: Array<number>,
    normalLines: Array<number>,
}

interface FailureResult {
    successful: false,
    error: string,
    diagnostics: Array<{index: number, message: string}>
}

export class Interpreter {
    ports: any;
    nextResultCallback?: (r: Result) => void;
    nextStateCallback?: (s: StateInfo) => void;

    constructor(ports: any) {
        this.ports = ports;

        // Bind results to call the next callback
        this.ports.result.subscribe((result: Result) => {
            if (this.nextResultCallback) {
                this.nextResultCallback(result);
            }
        });
        // Bind state to call the next callback
        this.ports.state.subscribe((state: StateInfo) => {
            if (this.nextStateCallback) {
                this.nextStateCallback(state);
            }
        });
    }

    /**
     * Update the code in the interpreter.
     * @param code the new code to use, separated by newlines
     * @returns a promise of the result of compilation
     * 
     * Note: this function is never called concurrently with step
     */
    async setCode(code: string): Promise<Result> {
        const promise: Promise<Result> = new Promise((resolve, _reject) => {
            this.nextResultCallback = resolve
        });

        this.ports.compile.send(code);

        return promise;
    }

    /**
     * Run one step of the interpreter
     * @returns the state at the end of that step
     * 
     * Note: this function is never called concurrently with setCode
     */
    async step(): Promise<StateInfo> {
        const promise: Promise<StateInfo> = new Promise((resolve, _reject) => {
            this.nextStateCallback = resolve
        });

        this.ports.step.send(true);

        return promise;
    }
}