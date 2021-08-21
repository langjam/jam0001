import * as React from "react";
import ReactDom from "react-dom"
import { Header } from "./components/header";
import { Editor } from "./components/editor";
import { Stack } from "./components/stack";
import { StateInfo } from "./interpreter";
import { Runtime } from "./runtime";
import { Body } from "./components/body";

function App() {
	const [state, setState] = React.useState<StateInfo | null>(null)

	const runtime = React.useMemo(() => new Runtime(setState), []);

	return (
		<div className='app'>
			<Header runtime={runtime} />
			<Body
				runtime={runtime}
				state={state}
			/>
		</div>	
	);
}

ReactDom.render(<App />, document.getElementById("app"));
