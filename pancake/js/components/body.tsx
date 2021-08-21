import { StateInfo } from "../interpreter";
import { Runtime } from "../runtime";
import { Editor } from "./editor";
import { Stack } from "./stack";

interface BodyProps {
    runtime: Runtime,
    state: StateInfo
}

export function Body({ runtime, state }: BodyProps) {

    return (
        <div className='content flex-row'>
            <div style={{width: '80%'}}>
                <Editor
                    runtime={runtime}
                    state={state}
                />
            </div>
            <div style={{width: '20%'}}>
                <Stack state={state} />
            </div>
        </div>
    )
}