import { Interpreter, Result, StateInfo } from './interpreter';

type Mode = 
    | { type: 'Stopped' }
    | { type: 'RunUnbounded' }
    | { type: 'RunForN', n: number };

export class Runtime {
    // Whether a run loop is currently going
    running: boolean;
    // The current mode of the runtime
    mode: Mode;
    delaySeconds: number = 0.5;
    interpreter: Interpreter = new Interpreter();
    stateListener: (s: StateInfo | null) => void;
    resultListener: (r: Result) => void;
    newCode: string | null = null;

    constructor(
        stateListener: (s: StateInfo) => void,
        resultListener: (r: Result) => void
    ) {
        this.stateListener = stateListener;
        this.resultListener = resultListener;
    }

    setDelaySeconds(delaySeconds: number) {
        this.delaySeconds = delaySeconds;
    }

    setCode(code: string) {
        this.newCode = code;
        this.stop();
    }

    start() {
        this.mode = { type: 'RunUnbounded' };
        this.runLoop();
    }

    step(n: number) {
        if (this.mode.type === 'RunForN') {
            this.mode.n += n;
        }
        else {
            this.mode = { type: 'RunForN', n };
        }
        this.runLoop();
    }

    stop() {
        this.mode = { type: 'Stopped' };
    }

    async runLoop() {
        if (this.running) {
            return;
        }
        this.running = true;

        if (this.newCode !== null) {
            const result = await this.interpreter.setCode(this.newCode);
            this.resultListener(result);
            this.newCode = null;
        }

        while (this.mode.type !== 'Stopped') {
            if (this.mode.type === 'RunForN') {
                if (this.mode.n === 0) {
                    this.stop();
                    break;
                }
                this.mode.n -= 1;
            }
            const newState = await this.interpreter.step();
            this.stateListener(newState);
            await delay(this.delaySeconds);
        }

        this.running = false;
    }
}

function delay(seconds: number) {
    return new Promise( resolve => setTimeout(resolve, seconds * 1000) );
}