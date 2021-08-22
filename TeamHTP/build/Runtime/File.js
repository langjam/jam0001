import { wrappedElementToMd } from '../Markdown';
async function commitToDisk(runtime) {
    const wrappedElements = runtime.getWrappedElements();
    for (const element of wrappedElements) {
        console.log(wrappedElementToMd(element));
    }
}
export { commitToDisk };
