import { Runtime } from "../runtime";

import icon from '../../static/pancake-logo.png';

interface HeaderProps {
    runtime: Runtime
}

export function Header({ runtime }: HeaderProps) {
    return (
        <div className='flex-row header'>
            <Logo />
            <Controls runtime={runtime} />
        </div>
    )
}

function Logo() {
    return (
        <div className='logo flex-row horizontal-rhythm-8'>
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
    return <div className='flex-row horizontal-rhythm-16' style={{ marginLeft: '128px'}}>
        <button onClick={runtime.start}>Run</button>
        <button onClick={() => runtime.step(1)}>Step</button>
        <button onClick={runtime.stop}>Stop</button>
        {/* TODO Slider for speed (use pageEvents.onChangeSpeed) */}
    </div>
}