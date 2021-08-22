import { Runtime } from "../runtime";

import icon from '../../static/pancake-logo.png';

interface HeaderProps {
    runtime: Runtime
}

export function Header({ runtime }: HeaderProps) {
    return (
        <div className='flex-row flex-align-center header'>
            <Logo />
            <Controls runtime={runtime} />
        </div>
    )
}

function Logo() {
    return (
        <div className='logo flex-row flex-align-center horizontal-rhythm-16'>
            <img src={icon} />
            <div>Pancake Playground</div> 
        </div>
    )
}

interface ControlsProps {
    runtime: Runtime
}

export function Controls({ runtime }: ControlsProps) {
    // TODO use flex-box row style and make look nice
    return <div className='flex-row flex-align-center horizontal-rhythm-16' style={{ marginLeft: '32px'}}>
        <button onClick={() => runtime.start()}>Run</button>
        <button onClick={() => runtime.step(1)}>Step</button>
        <button onClick={() => runtime.stop()}>Stop</button>
        <button onClick={() => runtime.reset()}>Clear</button>
        {/* TODO Slider for speed (use runtime.delaySeconds) */}
    </div>
}