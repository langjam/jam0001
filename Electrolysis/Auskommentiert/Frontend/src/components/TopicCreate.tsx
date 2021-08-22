import React from "react";
import { Component } from "react";
import { Link } from "react-router-dom";

import '../css/all.css'
import { TopicType } from "./types";


class TopicCreate extends Component<{}, {}> {
    private titleRef: React.RefObject<HTMLInputElement> = React.createRef()
    private bodyRef: React.RefObject<HTMLTextAreaElement> = React.createRef()


    render() {
        return (
            <div className="middle">
                <div className="vertical">
                    <label htmlFor="createTitle" className="vertical">Title: </label>
                    <input id="createTitle" ref={this.titleRef} className="vertical"></input>
                </div>
                <div className="vertical">
                    <label htmlFor="createBody" className="vertical">Body: </label>
                    <textarea className="max-width vertical" ref={this.bodyRef} id="createBody" rows={10}></textarea>
                </div>
                <div>
                    <Link to="/">
                        <button className="right" onClick={() => this.create()}>Create</button>
                    </Link>
                </div>
            </div >
        );
    }

    create() {
        let title = this.titleRef.current?.value ?? "";
        let content = this.bodyRef.current?.value ?? "";
        let body: TopicType = {
            id: "",
            title: title,
            content: content,
            children: [],
            date: Date.now(),
            upvotes: 0
        }
        fetch("http://" + window.location.hostname + ":6789/api/create_topic", {
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

export default TopicCreate