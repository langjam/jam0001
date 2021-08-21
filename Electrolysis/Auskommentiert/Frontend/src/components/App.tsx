import { Component } from 'react';
import '../css/App.css';
import { Topic } from './Topic';
import { BrowserRouter as Router, Route } from 'react-router-dom'
import { TopicViewer } from './TopicViewer';
import { AppType } from './types'
import TopicCreate from './TopicCreate';
import AnswerCreate from './AnswerCreate';
import AnswerCreateTopic from './AnswerCreateTopic';

let data: AppType = {
    topics: [
        {
            id: 1,
            heading: "Hi",
            body: "Test Body",
            upvotes: 0,
            comments: [
                {
                    topic: 1,
                    id: 1,
                    parent: 0,
                    position: 0,
                    body: "Ich bin ein Kommentar",
                    answers: [
                        {
                            topic: 1,
                            id: 2,
                            parent: 1,
                            position: 0,
                            body: "Unterkommentar",
                            answers: [],
                            upvotes: 0,
                            creation: Date.now()
                        },
                        {
                            topic: 1,
                            id: 3,
                            parent: 1,
                            position: 1,
                            body: "Zweiter Unterkommentar",
                            answers: [
                                {
                                    topic: 1,
                                    id: 4,
                                    parent: 3,
                                    position: 0,
                                    body: "Antwort",
                                    answers: [],
                                    upvotes: 0,
                                    creation: Date.now()
                                }
                            ],
                            upvotes: 0,
                            creation: Date.now()
                        }
                    ],
                    upvotes: 0,
                    creation: Date.now()
                }
            ],
            creation: Date.now()
        },
        {
            id: 2,
            heading: "Programming",
            body: "Body",
            upvotes: 0,
            comments: [
                {
                    topic: 2,
                    id: 1,
                    parent: 0,
                    position: 0,
                    body: "Ich bin ein Kommentar",
                    answers: [
                        {
                            topic: 2,
                            id: 2,
                            parent: 1,
                            position: 0,
                            body: "Unterkommentar",
                            answers: [],
                            upvotes: 0,
                            creation: Date.now()
                        }
                    ],
                    upvotes: 0,
                    creation: Date.now()
                }
            ],
            creation: Date.now()
        }
    ]
}

class App extends Component<{}, AppType> {

    UNSAFE_componentWillMount() {
        this.setState(data)
        //fetch("localhost:6789").then(data => data.json()).then(data => this.setState(data)).catch(reason => {})
    }

    render() {
        return (
            <div>
                <Router>
                    <Route exact path="/create_topic">
                        <TopicCreate></TopicCreate>
                    </Route>
                    <Route exact path="/create_answer_comment">
                        <AnswerCreate></AnswerCreate>
                    </Route>
                    <Route exact path="/create_answer_topic">
                        <AnswerCreateTopic></AnswerCreateTopic>
                    </Route>
                    {
                        this.state.topics.map(topic =>
                            <Route key={topic.id} exact path={"/topic/" + topic.id}>
                                <Topic id={topic.id} heading={topic.heading} body={topic.body} comments={topic.comments} upvotes={topic.upvotes} creation={topic.creation} />
                            </Route>
                        )
                    }
                    <Route exact path="/">
                        <TopicViewer topics={this.state.topics}></TopicViewer>
                    </Route>
                </Router>
            </div>
        );
    }
}

export default App;