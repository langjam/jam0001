import { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { CommentType } from './types'
import '../css/Comment.css';
import '../css/all.css';
import { Link } from 'react-router-dom';
import GlobalCommentStore from './GlobalCommentStore';



class Comment extends Component<CommentType, CommentType> {
    answers: Array<ReactElement<any, any>> = [];

    UNSAFE_componentWillMount() {
        this.setState(this.props)
        for (let entry of this.props.answers) {
            let component = <Comment id={entry.id} body={entry.body} answers={entry.answers} upvotes={entry.upvotes} creation={entry.creation}></Comment>
            this.answers.push(component)
        }
    }

    render() {
        return (
            <div className="">
                <div className="body comment">
                    <p className="content">{this.state.body}</p>
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
    }
    downvote() {
        this.setState({ upvotes: this.state.upvotes - 1 })
    }
}


export { Comment }
export type { CommentType }