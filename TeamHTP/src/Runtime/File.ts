import Runtime from './index'
import {wrappedElementToMd} from '../Markdown'

async function commitToDisk(runtime: Runtime) {
    const wrappedElements = runtime.getWrappedElements()
    for (const element of wrappedElements) {
        console.log(wrappedElementToMd(element))
    }
}

export {
    commitToDisk
}
