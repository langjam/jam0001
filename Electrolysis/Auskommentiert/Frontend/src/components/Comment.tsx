import { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { CommentType, Downvote, Upvote } from './types'
import '../css/Comment.css';
import '../css/all.css';
import { Link } from 'react-router-dom';
import GlobalCommentStore from './GlobalCommentStore';



class Comment extends Component<CommentType, CommentType> {
    answers: Array<ReactElement<any, any>> = [];

    constructor(props: CommentType) {
        super(props);
        this.state = props;
        this.initAnswers(this.props);
    }

    initAnswers(values: CommentType) {
        this.answers = [];
        for (let entry of values.children) {
            let component = <Comment id={entry.id} content={entry.content} children={entry.children} upvotes={entry.upvotes} date={entry.date}></Comment>
            this.answers.push(component)
        }
    }

    componentDidMount() {
        this.setState(this.props);
        this.initAnswers(this.state);
    }

    render() {
        return (
            <div className="" key={Date.now()}>
                <div className="body comment">
                    <p className="content">{this.state.content}</p>
                    <button onClick={() => this.upvote()}>{this.state.upvotes} &#8593;</button>
                    <button onClick={() => this.downvote()}>&#8595;</button>
                    <Link to="/create_answer_comment">
                        <button onClick={() => GlobalCommentStore.setComment(this.state)}>Answer</button>
                    </Link>
                    <button>Up</button>
                    <button>Down</button>
                </div>

                <div className="indent">
                    <>
                        {this.answers}
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


export { Comment }
export type { CommentType }