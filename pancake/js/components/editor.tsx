import { StateInfo } from "../interpreter";
import { Runtime } from "../runtime";

interface EditorProps {
    runtime: Runtime,
    state: StateInfo
}

export function Editor(props: EditorProps) {
    // on initialization start out empty or whatever
    // use props.pageEvents.onCodeChanged to notify of code changes
    // use the other props when they change (useEffect/useMemo for that) to alter the editor highlights

    return (
        <div>
            <h1>Editor</h1>
        </div>
    )
}