
export interface StateInfo {
    currentLine: number;
    activeLines: number[];
}

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

    constructor() {
        const ports: any = {};

        ports.result.subscribe((result: Result) => {
            if (this.nextResultCallback) {
                this.nextResultCallback(result);
            }
        });
        ports.state.subscribe((state: StateInfo) => {
            if (this.nextStateCallback) {
                this.nextStateCallback(state);
            }
        });
    }

    async setCode(code: string): Promise<Result> {
        const promise: Promise<Result> = new Promise((resolve, _reject) => {
            this.nextResultCallback = resolve
        });

        this.ports.compile(code);

        return promise;
    }

    async step(): Promise<StateInfo> {
        const promise: Promise<StateInfo> = new Promise((resolve, _reject) => {
            this.nextStateCallback = resolve
        });

        this.ports.step();

        return promise;
    }
}