import { PageEvents } from "..";

interface EditorProps {
    pageEvents: PageEvents,
    currentLine: number,
    activeCodeLines: number[]
}

export function Editor(props: EditorProps) {
    // on initialization start out empty or whatever
    // use props.pageEvents.onCodeChanged to notify of code changes
    // use the other props when they change (useEffect/useMemo for that) to alter the editor highlights

    return (
        <div>Hazah!</div>
    )
}