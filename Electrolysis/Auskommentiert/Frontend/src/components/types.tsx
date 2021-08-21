

type AppType = {
    topics: TopicType[]
}

type TopicType = {
    id: number
    heading: string,
    body: string,
    comments: CommentType[],
    upvotes: number,
    creation: number
}

type CommentType = {
    topic: number,
    id: number,
    parent: number,
    position: number,
    body: string,
    answers: CommentType[],
    upvotes: number,
    creation: number
}

export type { AppType, TopicType, CommentType }