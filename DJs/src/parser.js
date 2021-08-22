const { notes } = require('./sound')

function parse(content) {
    let inComment = false
    let nextLineFunc = false

    const data = {}
    let currentData = {
        variables: {},
        waveType: null,
    }

    content.toString().split('\n').forEach((line) => {
        if (nextLineFunc) {
            nextLineFunc = false
            const funcName = line.match(/function (?<functionName>[a-zA-Z]*)/).groups.functionName
            data[funcName] = currentData
            currentData = {variables: {}, waveType: null}

        }

        if (line.includes('/**')) {
            inComment = true
        }
        if (line.includes(' */')) {
            nextLineFunc = true
            inComment = false
        }

        if (inComment) {

            const regexRes = line.match(/@(?<effect>[a-zA-Z]*) (?<target>[a-zA-Z0-9]*) ?(?<parameters>.*)?/)

            if (null === regexRes) {
                return
            }

            const groups = regexRes.groups

            switch(groups.effect) {
                case 'var':
                    const regex = /(?<note>[a-zA-Z#]+) ?(?<octave>\d)?/g
                    let matches = []
                    let parameters = [];
                    while (matches = regex.exec(groups.parameters)) {
                        if ((matches.groups.octave && (matches.groups.octave < 0 || matches.groups.octave > 9)) || !notes.has(matches.groups.note)) {
                            return
                        }
                        const octave = matches.groups.octave ? parseInt(matches.groups.octave) : 0
                        const parameter = notes.get(matches.groups.note)[octave];
                        parameters.push(parameter);
                    }
                    currentData.variables[groups.target] = {"frequencies" : parameters}

                    break
                case 'waveType':
                    currentData.waveType = groups.target
                    break
                }
        }
    })
    return data
}

module.exports = { parse }
