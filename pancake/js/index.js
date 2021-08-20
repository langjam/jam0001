import React, { Component } from "react";
import { render } from "react-dom"

class App extends Component {
	render() {
		return(<p>It works!</p>)
	}
}

render(<App />, document.getElementById("app"))