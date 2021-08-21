import * as React from "react";
import { render } from "react-dom"
import { Controls } from "./components/controls";
import { Editor } from "./components/editor";
import { Stack } from "./components/stack";
import { StateInfo } from "./interpreter";
import { Runtime } from "./runtime";

function App() {
	const [state, setState] = React.useState<StateInfo | null>(null)

	const runtime = React.useMemo(() => new Runtime(setState), []);

	return (
		<div>
			<Controls runtime={runtime} />
			<Editor
				runtime={runtime}
				state={state}
			/>
			<Stack state={state} />
			<p>It works!</p>
		</div>	
	);
}

render(<App />, document.getElementById("app"))