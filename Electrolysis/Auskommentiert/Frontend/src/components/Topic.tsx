import React, { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { CommentType, Comment } from './Comment';


type TopicType = {
    heading: string,
    body: string,
    comments: CommentType[],
    creation: number
}

class Topic extends Component<TopicType, TopicType> {
    comments: Array<ReactElement<any, any>> = [];

    componentWillMount() {
        this.setState(this.props)
        for (let entry of this.props.comments) {
            let component = <Comment body={entry.body} answers={entry.answers} creation={entry.creation}></Comment>
            this.comments.push(component)
        }
    }

    render() {
        return (
            <div className="App">
                <div>
                    <h2>{this.state.heading}</h2>
                    <h2>{new Date(this.state.creation).toLocaleString()}</h2>
                </div>

                <h3>Kommentare</h3>
                <>
                    {this.comments}
                </>
            </div>
        );
    }
}

export { Topic }
export type { TopicType }
