import { Runtime } from "../runtime";

interface IOPanelProps {
    runtime: Runtime
}

export function IOPanel({ runtime }: IOPanelProps) {
    return (
        <div style={{ textAlign: 'center'}}>
            <h1>IO Panel</h1>
        </div>
    )
}
