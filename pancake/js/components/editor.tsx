import * as React from "react";
import AceEditor from "react-ace";

import "ace-builds/src-noconflict/mode-python";
import "ace-builds/src-noconflict/theme-gruvbox";
import "ace-builds/src-noconflict/ext-language_tools";

import { Result, StateInfo } from "../interpreter";
import { Runtime } from "../runtime";
import { Ace } from "ace-builds";

interface EditorProps {
    runtime: Runtime,
    result: Result,
    state: StateInfo
}

export function Editor({ runtime, state }: EditorProps) {
    // on initialization start out empty or whatever
    // use props.pageEvents.onCodeChanged to notify of code changes
    // use the other props when they change (useEffect/useMemo for that) to alter the editor highlights

    React.useEffect(() => runtime.setCode(defaultValue), []);

    return (
        <div style={{ textAlign: 'center'}}>
            <h1>Editor</h1>
            <AceEditor
                defaultValue={defaultValue}
                setOptions={{
                    hScrollBarAlwaysVisible: true,
                    showPrintMargin: false,
                    enableBasicAutocompletion: true,
                    enableSnippets: true,
                    enableLiveAutocompletion: true
                }}
                onLoad={onLoad}
                style={{ width: '100%' }}
                mode="python"
                theme="gruvbox"
                onChange={(value, _e) => runtime.setCode(value)}
                editorProps={{ $blockScrolling: true }}
            />
        </div>
    )
}

const knownTokens: string[] = [
    'write', 'read', 'str', 'num', 'flip', 'if', 'exit'
]

function onLoad(editor: Ace.Editor) {
    const mode = editor.getSession().getMode();

    mode.getCompletions = (_state: string, _session: Ace.EditSession, _pos: Ace.Point, _prefix: string) => {
        return knownTokens.map((token) => {
            const completion: Ace.Completion = {
                value: token,
                meta: "Keyword",
                score: 1,
                caption: token || ""
            };
            return completion;
        })
    }
}

const defaultValue = `\
1
# 3
2
3
<
flip_if
4
5
# 2
# flip
+
halt
`;