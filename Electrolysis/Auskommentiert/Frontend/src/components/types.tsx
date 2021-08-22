

type AppType = {
    topics: TopicType[]
}

type TopicType = {
    id: string
    title: string,
    content: string,
    children: CommentType[],
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

type CommentCreate = {
    parent: string,
    comment: CommentType
}

type Upvote = {
    id: string,
}

type Downvote = {
    id: string,
}

type SwapRequest = {
    post: string,
    id: string,
}

type DeleteRequest = {
    id: string,
}

export type { AppType, TopicType, CommentType, CommentCreate, Upvote, Downvote, SwapRequest, DeleteRequest }