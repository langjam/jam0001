import { Interpreter, Result, StateInfo } from './interpreter';

type Mode = 
    | { type: 'Idle' }
    | { type: 'Compile', code: string }
    | { type: 'Error' }
    | { type: 'RunUnbounded' }
    | { type: 'RunForN', n: number };

export class Runtime {
    // Whether a run loop is currently going
    running: boolean;
    // The current mode of the runtime
    mode: Mode = { type: 'Idle' };
    delaySeconds: number = 0.5;
    interpreter: Interpreter;
    stateListener: (s: StateInfo | null) => void;
    resultListener: (r: Result) => void;
    
    lastCode: string;

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

    reset() {
        this.mode = { type: 'Compile', code: this.lastCode };
        this.runLoop();
    }

    setCode(code: string) {
        this.mode = { type: 'Compile', code };
        this.lastCode = code;
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
        this.mode = { type: 'Idle' };
    }

    async runLoop() {
        if (this.running) {
            console.log('Loop already running');
            return;
        }
        this.running = true;

        console.log('Checking for compile');
        if (this.mode.type === 'Compile') {
            console.log('New code');
            const result = await this.interpreter.setCode(this.mode.code);
            this.stateListener(null);
            if (!result.successful) {
                console.log('Compilation Error')
                this.mode = { type: 'Error' };
            }
            else {
                console.log('Compilation Successful')
                this.mode = { type: 'Idle' };
            }
            console.log(result);
            this.resultListener(result);
            this.running = false;
            return;
        }

        console.log('Checking for steps');
        while (this.mode.type === 'RunUnbounded' || this.mode.type === 'RunForN') {
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