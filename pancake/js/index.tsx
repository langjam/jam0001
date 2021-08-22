import * as React from "react";
import ReactDom from "react-dom"

import { Result, StateInfo } from "./interpreter";
import { Runtime } from "./runtime";
import { Header } from "./components/header";
import { Body } from "./components/body";

function App() {
	const [result, setResult] = React.useState<Result | null>(null);
	const [state, setState] = React.useState<StateInfo | null>(null);

	const runtime = React.useMemo(() => new Runtime(setState, setResult), []);

	return (
		<div className='app'>
			<Header runtime={runtime} />
			<Body
				runtime={runtime}
				result={result}
				state={state}
			/>
		</div>	
	);
}

ReactDom.render(<App />, document.getElementById("app"));
