import Runtime from './index'
import {Function} from "../Types";

async function commitToDisk(runtime: Runtime) {
    const wrappedElements = runtime.getWrappedElements()
    for (const element of wrappedElements) {
        if (element instanceof Function) {

        }
    }
}

export {
    commitToDisk
}
