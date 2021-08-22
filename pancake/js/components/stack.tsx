import { StateInfo } from "../interpreter";

interface StackProps {
    state: StateInfo
}

export function Stack(props: StackProps) {
    return (
        <div style={{ textAlign: 'center'}}>
            <h1>Stack</h1>
        </div>
    )
}
