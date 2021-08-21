import { Runtime } from "../runtime";

interface ControlsProps {
    runtime: Runtime
}

export function Controls({ runtime }: ControlsProps) {
    // TODO use flex-box row style and make look nice
    return <div>
        <button onClick={() => runtime.step(1)}>Step</button>
        <button onClick={runtime.start}>Run</button>
        <button onClick={runtime.stop}>Stop</button>
        {/* TODO Slider for speed (use pageEvents.onChangeSpeed) */}
    </div>
}
