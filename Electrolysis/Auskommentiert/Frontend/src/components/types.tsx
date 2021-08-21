

type AppType = {
    topics: TopicType[]
}

type TopicType = {
    id: string
    title: string,
    content: string,
    comments: CommentType[],
    upvotes: number,
    date: number
}

type CommentType = {
    id: string,
    content: string,
    children: CommentType[],
    upvotes: number,
    date: number
}

export type { AppType, TopicType, CommentType }