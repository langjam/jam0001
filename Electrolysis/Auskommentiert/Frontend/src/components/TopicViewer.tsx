import { Component } from "react";
import { Link } from 'react-router-dom'
import { AppType } from './types'

class TopicViewer extends Component<AppType, AppType> {

    componentWillMount() {
        this.setState(this.props)
    }

    render() {
        return <div>
            {this.state.topics.map(topic =>
                <Link to={"topic/" + topic.id}>
                    <p>{topic.heading}</p>
                </Link>
            )}
        </div>

    }
}

export { TopicViewer }