import { Interpreter, Result, StateInfo } from './interpreter';

type Mode = 
    | { type: 'Stopped' }
    | { type: 'Error' }
    | { type: 'RunUnbounded' }
    | { type: 'RunForN', n: number };

export class Runtime {
    // Whether a run loop is currently going
    running: boolean;
    // The current mode of the runtime
    mode: Mode = { type: 'Stopped' };
    delaySeconds: number = 0.5;
    interpreter: Interpreter;
    stateListener: (s: StateInfo | null) => void;
    resultListener: (r: Result) => void;
    newCode: string | null = null;

    constructor(
        ports: any,
        stateListener: (s: StateInfo) => void,
        resultListener: (r: Result) => void
    ) {
        this.interpreter = new Interpreter(ports);
        this.stateListener = stateListener;
        this.resultListener = resultListener;
    }

    setDelaySeconds(delaySeconds: number) {
        this.delaySeconds = delaySeconds;
    }

    setCode(code: string) {
        this.newCode = code;
        this.runLoop();
    }

    start() {
        if (this.mode.type === 'Error') {
            return;
        }
        this.mode = { type: 'RunUnbounded' };
        this.runLoop();
    }

    step(n: number) {
        if (this.mode.type === 'Error') {
            return;
        }
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
            console.log('New code');
            const result = await this.interpreter.setCode(this.newCode);
            if (!result.successful) {
                console.log('Compilation Error')
                this.mode = { type: 'Error' };
            }
            else {
                if (this.mode.type === 'Error') {
                    this.mode = { type: 'Stopped' };
                }
            }
            console.log(result);
            this.resultListener(result);
            this.newCode = null;
        }

        while (this.mode.type !== 'Stopped' && this.mode.type !== 'Error') {
            console.log(this.mode);
            console.log('Performing step');
            const newState = await this.interpreter.step();
            console.log('Step completed');
            console.log(newState);
            this.stateListener(newState);
            if (this.mode.type === 'RunForN') {
                this.mode.n -= 1;
                if (this.mode.n === 0) {
                    this.stop();
                    break;
                }
            }
            await delay(this.delaySeconds);
        }

        this.running = false;
    }
}

function delay(seconds: number) {
    return new Promise( resolve => setTimeout(resolve, seconds * 1000) );
}