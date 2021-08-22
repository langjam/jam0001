import { Component } from 'react';
import '../css/App.css';
import { Topic } from './Topic';
import { BrowserRouter as Router, Route } from 'react-router-dom'
import { TopicViewer } from './TopicViewer';
import { AppType } from './types'
import TopicCreate from './TopicCreate';
import AnswerCreate from './AnswerCreate';
import AnswerCreateTopic from './AnswerCreateTopic';

//let data: AppType = {
//    topics: [
//        {
//            id: "1",
//            title: "Hi",
//            content: "Test Body",
//            upvotes: 0,
//            comments: [
//                {
//                    id: "1",
//                    content: "Ich bin ein Kommentar",
//                    children: [
//                        {
//                            id: "2",
//                            content: "Unterkommentar",
//                            children: [],
//                            upvotes: 0,
//                            date: Date.now()
//                        },
//                        {
//                            id: "3",
//                            content: "Zweiter Unterkommentar",
//                            children: [
//                                {
//                                    id: "4",
//                                    content: "Antwort",
//                                    children: [],
//                                    upvotes: 0,
//                                    date: Date.now()
//                                }
//                            ],
//                            upvotes: 0,
//                            date: Date.now()
//                        }
//                    ],
//                    upvotes: 0,
//                    date: Date.now()
//                }
//            ],
//            date: Date.now()
//        },
//        {
//            id: "2",
//            title: "Programming",
//            content: "Body",
//            upvotes: 0,
//            comments: [
//                {
//                    id: "1",
//                    content: "Ich bin ein Kommentar",
//                    children: [
//                        {
//                            id: "2",
//                            content: "Unterkommentar",
//                            children: [],
//                            upvotes: 0,
//                            date: Date.now()
//                        }
//                    ],
//                    upvotes: 0,
//                    date: Date.now()
//                }
//            ],
//            date: Date.now()
//        }
//    ]
//}
let websocket: WebSocket = new WebSocket("ws://" + window.location.hostname + ":6789/api/ws/data")

class App extends Component<{}, AppType> {


    constructor(props: any) {
        super(props);
        this.state = {
            topics: []
        }
    }

    componentDidMount() {
        fetch("http://" + window.location.hostname + ":6789/api/data").then(data => data.json()).then(data => {
            this.setState(data);
        }).catch(reason => { console.log(reason) })

        websocket.addEventListener('message', (msg) => {
            let obj = JSON.parse(msg.data.toString())
            this.setState(obj);
        });
    }

    render() {
        return (
            <div key={Date.now()}>
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
                                <Topic id={topic.id} title={topic.title} content={topic.content} children={topic.children} upvotes={topic.upvotes} date={topic.date} />
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