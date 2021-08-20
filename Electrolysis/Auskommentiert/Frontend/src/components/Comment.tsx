import { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';


type CommentType = {
    body: string,
    answers: CommentType[],
    creation: number
}

class Comment extends Component<CommentType, CommentType> {
    answers: Array<ReactElement<any, any>> = [];

    componentWillMount() {
        this.setState(this.props)
        for (let entry of this.props.answers) {
            let component = <Comment body={entry.body} answers={entry.answers} creation={entry.creation}></Comment>
            this.answers.push(component)
        }
    }

    render() {
        return (
            <div className="App">
                <div className="App">
                    <p>{this.state.body}</p>
                </div>

                <div className="App">
                    <>
                        {this.answers}
                    </>
                </div>
            </div>
        );
    }
}


export { Comment }
export type { CommentType }