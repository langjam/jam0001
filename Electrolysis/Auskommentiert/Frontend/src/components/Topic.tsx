import React, { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { Comment } from './Comment';
import { TopicType } from './types'
import '../css/Topic.css'
import '../css/all.css'
import { Link } from 'react-router-dom';
import GlobalTopicStore from './GlobalTopicStore';


class Topic extends Component<TopicType, TopicType> {
    comments: Array<ReactElement<any, any>> = [];

    UNSAFE_componentWillMount() {
        this.setState(this.props)
        for (let entry of this.props.comments) {
            let component = <Comment id={entry.id} body={entry.body} answers={entry.answers} upvotes={entry.upvotes} creation={entry.creation}></Comment>
            this.comments.push(component)
        }
    }

    render() {
        return (
            <div className="middle">
                <div className="">
                    <h2 className="heading_element">{this.state.heading}</h2>
                    <h4 className="heading_element">{new Date(this.state.creation).toLocaleString()}</h4>
                </div>
                <div className="body">
                    <p className="content">{this.state.body}</p>
                </div>
                <div>
                    <button onClick={() => this.upvote()}>{this.state.upvotes} &#8593;</button>
                    <button onClick={() => this.downvote()}>&#8595;</button>
                    <Link to="/create_answer_topic">
                        <button onClick={() => GlobalTopicStore.setTopic(this.state)}>Answer</button>
                    </Link>
                </div>
                <h3>Comments:</h3>
                <>
                    {this.comments}
                </>
            </div>
        );
    }

    upvote() {
        this.setState({ upvotes: this.state.upvotes + 1 })
    }
    downvote() {
        this.setState({ upvotes: this.state.upvotes - 1 })
    }
}

export { Topic }
export type { TopicType }
