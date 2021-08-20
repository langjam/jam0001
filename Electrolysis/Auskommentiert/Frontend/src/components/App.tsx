import React, { ReactElement } from 'react';
import { Component } from 'react';
import '../css/App.css';
import { Topic, TopicType } from './Topic';

type AppType = {
    topics: TopicType[]
}

let data: AppType = {
    topics: [
        {
            heading: "Hi",
            body: "Test Body",
            comments: [
                {
                    body: "Ich bin ein Kommentar",
                    answers: [
                        {
                            body: "Unterkommentar",
                            answers: [],
                            creation: Date.now()
                        }
                    ],
                    creation: Date.now()
                }
            ],
            creation: Date.now()
        }
    ]
}

class App extends Component<{}, AppType> {
    topics: Array<ReactElement<any, any>> = [];

    componentWillMount() {
        this.setState(data)
        for (let entry of data.topics) {
            let component = <Topic heading={entry.heading} body={entry.body} comments={entry.comments} creation={entry.creation}></Topic>
            this.topics.push(component)
        }
        //fetch("localhost:6789").then(data => data.json()).then(data => this.setState(data)).catch(reason => {})
    }

    render() {
        return (
            <div className="App">
                <>
                    {this.topics}
                </>
            </div>
        );
    }
}

export default App;
