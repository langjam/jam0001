import readline from 'readline';
const rli = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
});
async function startRepl(callback) {
    while (true) {
        callback(await question(rli));
    }
}
function question(rli) {
    return new Promise((resolve, reject) => {
        rli.question('>>> ', (response) => {
            resolve(response);
        });
    });
}
export { startRepl };
