import { CommentType } from "./types"


const GlobalCommentStore = (() => {
    let _comment: CommentType
    return {
        getComment: () => _comment,
        setComment: (comment: CommentType) => _comment = comment
    }
})()

export default GlobalCommentStore