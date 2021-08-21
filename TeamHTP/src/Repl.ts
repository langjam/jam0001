import readline from 'readline'

const rli = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
})

async function startRepl(callback: (input: string) => void) {
    while (true) {
        callback(await question(rli))
    }
}

function question(rli: readline.Interface): Promise<string> {
    return new Promise<string>((resolve, reject) => {
        rli.question('>>> ', (response) => {
            resolve(response)
        })
    })
}

export {
    startRepl
}
