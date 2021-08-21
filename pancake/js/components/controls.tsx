import { PageEvents } from '../index';

interface ControlsProps {
    pageEvents: PageEvents
}

export function Controls({ pageEvents }: ControlsProps) {
    // TODO use flex-box row style and make look nice
    return <div>
        <button onClick={pageEvents.onStep}>Step</button>
        <button onClick={pageEvents.onRun}>Run</button>
        <button onClick={pageEvents.onStop}>Stop</button>
        {/* TODO Slider for speed (use pageEvents.onChangeSpeed) */}
    </div>
}
