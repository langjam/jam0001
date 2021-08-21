import { Component } from "react";

import '../css/all.css'


class TopicCreate extends Component<{}, {}> {

    render() {
        return (
            <div className="middle">
                <div className="vertical">
                    <label htmlFor="createTitle" className="vertical">Title: </label>
                    <input id="createTitle" className="vertical"></input>
                </div>
                <div className="vertical">
                    <label htmlFor="createBody" className="vertical">Body: </label>
                    <textarea className="max-width vertical" id="createBody" rows={10}></textarea>
                </div>
                <div>
                    <button className="right">Create</button>
                </div>
            </div>
        );
    }
}

export default TopicCreate