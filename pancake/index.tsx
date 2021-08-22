import * as React from "react";
import ReactDom from "react-dom";

import { Elm } from './elm/src/Main.elm';

import { Result, StateInfo } from "./js/interpreter";
import { Runtime } from "./js/runtime";
import { Header } from "./js/components/header";
import { Body } from "./js/components/body";

function App() {
	const [result, setResult] = React.useState<Result | null>(null);
	const [state, setState] = React.useState<StateInfo | null>(null);

	const ports = React.useMemo(() => Elm.Main.init().ports, []);
	const runtime = React.useMemo(() => new Runtime(ports, setState, setResult), []);

    const classNames = state?.universe === 'comment'
        ? 'app app-comment'
        : 'app';

	return (
		<div className={classNames}>
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
