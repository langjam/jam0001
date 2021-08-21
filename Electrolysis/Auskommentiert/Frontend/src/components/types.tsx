

type AppType = {
    topics: TopicType[]
}

type TopicType = {
    id: string
    heading: string,
    body: string,
    comments: CommentType[],
    upvotes: number,
    creation: number
}

type CommentType = {
    id: string,
    body: string,
    answers: CommentType[],
    upvotes: number,
    creation: number
}

export type { AppType, TopicType, CommentType }