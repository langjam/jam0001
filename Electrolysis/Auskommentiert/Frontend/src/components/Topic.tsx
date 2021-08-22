import React, { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { Comment } from './Comment';
import { Downvote, TopicType, Upvote } from './types'
import '../css/Topic.css'
import '../css/all.css'
import { Link } from 'react-router-dom';
import GlobalTopicStore from './GlobalTopicStore';


class Topic extends Component<TopicType, TopicType> {
    comments: Array<ReactElement<any, any>> = [];

    constructor(props: TopicType) {
        super(props);
        this.state = this.props;
        this.initComments(this.state);
    }

    initComments(values: TopicType) {
        this.comments = [];
        for (let entry of values.children) {
            let component = <Comment id={entry.id} content={entry.content} children={entry.children} upvotes={entry.upvotes} date={entry.date}></Comment>
            this.comments.push(component)
        }
    }

    componentDidMount() {
        GlobalTopicStore.setTopic(this.state);
        this.setState(this.props);
        this.initComments(this.props);
    }

    render() {
        return (
            <div className="middle" key={Date.now()}>
                <div className="">
                    <h2 className="heading_element">{this.state.title}</h2>
                    <h4 className="heading_element">{new Date(this.state.date).toLocaleString()}</h4>
                </div>
                <div className="body">
                    <p className="content">{this.state.content}</p>
                </div>
                <div>
                    <button onClick={() => this.upvote()}>{this.state.upvotes} &#8593;</button>
                    <button onClick={() => this.downvote()}>&#8595;</button>
                    <Link to="/create_answer_topic">
                        <button onClick={() => GlobalTopicStore.setTopic(this.state)}>Answer</button>
                    </Link>
                </div>
                <div key={Date.now()}>
                    <h3>Comments:</h3>
                    <>
                        {this.comments}
                    </>
                </div>
            </div>
        );
    }

    upvote() {
        this.setState({ upvotes: this.state.upvotes + 1 })
        let body: Upvote = {
            id: this.state.id
        }
        fetch("http://" + window.location.hostname + ":6789/api/upvote", {
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

    downvote() {
        this.setState({ upvotes: this.state.upvotes - 1 })
        let body: Downvote = {
            id: this.state.id
        }
        fetch("http://" + window.location.hostname + ":6789/api/downvote", {
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

export { Topic }
export type { TopicType }
