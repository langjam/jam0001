import Runtime from './index'
import {wrappedElementToMd} from '../Markdown'

function serializeRuntime(runtime: Runtime) {
    let serialized = ''
    const wrappedElements = runtime.getWrappedElements()
    for (const element of wrappedElements) {
        serialized += wrappedElementToMd(element)
    }
    return serialized
}

async function saveToDisk(runtime: Runtime) {
    console.log(serializeRuntime(runtime))
    // TODO: save serialized to disk
}

export {
    saveToDisk
}
