import { Component } from "react";

import '../css/all.css'
import { TopicType } from "./types";


class AnswerCreateTopic extends Component<{}, TopicType> {

    render() {
        return (
            <div className="middle">
                <div className="vertical">
                    <label htmlFor="createAnswer" className="vertical">Answer: </label>
                    <textarea className="max-width vertical" id="createAnswer" rows={10}></textarea>
                </div>
                <div>
                    <button className="right">Create</button>
                </div>
            </div>
        );
    }
}

export default AnswerCreateTopic