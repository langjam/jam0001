import * as React from "react";
import { render } from "react-dom"
import { Controls } from "./components/controls";
import { Editor } from "./components/editor";
import { StateInfo } from "./interpreter";

export interface PageEvents {
	onCodeChanged(newCode: string[]),
	onStep(): void,
	onRun(): void,
	onStop(): void,
	onChangeSpeed(speed: number): void
}

function App() {
	const [state, setState] = React.useState<StateInfo | null>(null)

	const events: PageEvents = {
		onCodeChanged(newCode: string[]) {

		},
		onStep() {

		},
		onRun() {

		},
		onStop() {

		},
		onChangeSpeed(speed: number) {

		}
	}

	return (
		<div>
			<Controls pageEvents={events} />
			<Editor
				pageEvents={events}
				currentLine={0}
				activeCodeLines={[]}
			/>
			<p>It works!</p>
		</div>	
	);
}

render(<App />, document.getElementById("app"))