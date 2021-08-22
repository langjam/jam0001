import { wrappedElementToMd } from '../Markdown';
function serializeRuntime(runtime) {
    let serialized = '';
    const wrappedElements = runtime.getWrappedElements();
    for (const element of wrappedElements) {
        serialized += wrappedElementToMd(element) + '\n';
    }
    return serialized;
}
async function saveToDisk(runtime) {
    console.log(serializeRuntime(runtime));
    // TODO: save serialized to disk
}
export { saveToDisk };
