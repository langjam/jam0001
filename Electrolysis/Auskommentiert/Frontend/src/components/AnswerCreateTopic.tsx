import React from "react";
import { Component } from "react";

import '../css/all.css'
import GlobalTopicStore from "./GlobalTopicStore";
import { CommentCreate, TopicType } from "./types";


class AnswerCreateTopic extends Component<{}, TopicType> {

    private bodyRef: React.RefObject<HTMLTextAreaElement> = React.createRef();

    render() {
        return (
            <div className="middle">
                <div className="vertical">
                    <label htmlFor="createAnswer" className="vertical">Answer: </label>
                    <textarea className="max-width vertical" ref={this.bodyRef} id="createAnswer" rows={10}></textarea>
                </div>
                <div>
                    <button className="right" onClick={() => this.createAnswer()}>Create</button>
                </div>
            </div>
        );
    }

    createAnswer() {
        let content = this.bodyRef.current?.value ?? "";
        let body: CommentCreate = {
            parent: GlobalTopicStore.getTopic().id,
            comment: {
                id: "",
                children: [],
                date: Date.now(),
                content: content,
                upvotes: 0
            }
        }
        fetch("http://" + window.location.hostname + ":6789/api/create_comment", {
            method: 'POST',
            mode: 'cors',
            cache: 'no-cache',
            headers: {
                'Content-Type': 'application/json',
            },
            redirect: 'follow',
            body: JSON.stringify(body),
        })
    }
}

export default AnswerCreateTopic