import { Component } from "react";

import '../css/all.css'
import GlobalCommentStore from "./GlobalCommentStore";
import { TopicType } from "./types";


class AnswerCreate extends Component<{}, TopicType> {

    UNSAFE_componentWillMount() {
        console.log(GlobalCommentStore.getComment())
    }

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

export default AnswerCreate