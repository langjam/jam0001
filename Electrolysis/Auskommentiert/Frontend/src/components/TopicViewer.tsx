import { Component } from "react";
import { Link } from 'react-router-dom'
import { AppType } from './types'

class TopicViewer extends Component<AppType, AppType> {


    constructor(props: AppType) {
        super(props);
        this.state = props;
    }

    componentDidMount() {
        this.setState(this.props)
    }

    render() {
        return <div key={this.state.topics.length}>
            <div>
                <Link to="create_topic">
                    <button>Create Topic</button>
                </Link>
            </div>
            <div>
                <h2>Topics</h2>
            </div>
            <div key={this.state.topics.length}>
                {this.state.topics.map(topic =>
                    <Link to={"topic/" + topic.id} key={topic.id}>
                        <p>{topic.title}</p>
                    </Link>
                )}
            </div>
        </div >

    }
}

export { TopicViewer }