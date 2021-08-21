import { TopicType } from "./types"


const GlobalTopicStore = (() => {
    let _topic: TopicType
    return {
        getTopic: () => _topic,
        setTopic: (topic: TopicType) => _topic = topic
    }
})()


export default GlobalTopicStore