import { StateInfo } from "../interpreter";

interface StackProps {
    state: StateInfo
}

export function Stack(props: StackProps) {
    return (
        <div>
            <h1>Stack</h1>
        </div>
    )
}
