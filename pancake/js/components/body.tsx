import { StateInfo } from "../interpreter";
import { Runtime } from "../runtime";
import { Editor } from "./editor";
import { IOPanel } from "./iopanel";
import { Stack } from "./stack";

interface BodyProps {
    runtime: Runtime,
    state: StateInfo
}

export function Body({ runtime, state }: BodyProps) {

    return (
        <div className='content flex-row'>
            <div style={{width: '50%'}}>
                <Editor
                    runtime={runtime}
                    state={state}
                />
            </div>
            <div style={{width: '25%', backgroundColor: 'black'}}>
                <IOPanel runtime={runtime} />
            </div>
            <div style={{width: '25%'}}>
                <Stack state={state} />
            </div>
        </div>
    )
}