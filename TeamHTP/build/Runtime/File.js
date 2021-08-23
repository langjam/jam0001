import { wrappedElementToMd } from '../Markdown';
import fs from 'fs';
function serializeRuntime(runtime) {
    let serialized = '';
    const wrappedElements = runtime.getWrappedElements();
    for (const element of wrappedElements) {
        serialized += wrappedElementToMd(element) + '\n';
    }
    return serialized;
}
async function saveToDisk(path, runtime) {
    const serialized = serializeRuntime(runtime);
    //console.log(serialized)
    fs.writeFileSync(path, serialized);
}
export { saveToDisk };
