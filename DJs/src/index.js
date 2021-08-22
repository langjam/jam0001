const fs = require('fs');
const path = require('path');

const {parse} = require('./parser.js')
const DESTINATION_FILE = "djs_output.js"
const originalFilePath = process.argv[2];


let currentPath = '.';

const getFileContent = (path) => fs.readFileSync(path, 'utf-8');
const getSoxPath = () => {
    if (process.argv.length > 3) {
        return process.argv[3];
    }
    return "sox"
};

const soxPath = getSoxPath()

/**
 *
 * @param {string} line
 * @param currentFunction
 */
const getFunctionName = (line, currentFunction) => {
    const functionNameGroups = line.match(/function (?<functionName>[a-zA-Z]*)/)
    return functionNameGroups ? functionNameGroups.groups.functionName : currentFunction;
}

function writeSoundLines(line, currentFunctionParameters) {
    if (!currentFunctionParameters) {
        return [];
    }
    toCheck = `\\b(?<=^([^\"]|\"[^\"]*\")*)(${Object.keys(currentFunctionParameters.variables).join('|')})\\b`;
    var re = new RegExp(toCheck, "g");
    if (!line.match(re)) {
        return [];
    }

    let waveType = currentFunctionParameters.waveType === null ? "sine" : currentFunctionParameters.waveType
    return line.match(re).map((matchedVariable) => {
        return `playNote('${soxPath}', [${currentFunctionParameters.variables[matchedVariable].frequencies}], typeof ${matchedVariable} === 'undefined' ? null : ${matchedVariable}, '${waveType}', \`${line}\`, '${matchedVariable}')`
    })
}


const transcompileFile = (filePath) => {
    const fileData = parse(getFileContent(filePath));
    const fileContent = getFileContent(filePath);
    let compiledFile = [`
    const execSync = require('child_process').execSync;
    const types = [0.125, 0.25, 0.5, 1, 1.5, 2]

    function playNote(path, frequencies, variable, waveType, line, variableName) {
        let tempo
        switch(typeof variable) {
            case 'string':
            case 'array':
              tempo = variable.length % 6
              break
            case 'number':
              tempo = variable % 6
              break
            default:
              tempo = Math.floor(Math.random() * 6)
              break
          }
        const duration = types[tempo]
        var isWin = process.platform === "win32";
        if (isWin) {
            soxCommand = \`\${path} -n -t waveaudio -q synth \${duration}\`
        } else {
            soxCommand = \`\${path} -n -d -q synth \${duration}\`
        }
        frequencies.forEach((frequence) => {
            if (frequence === 0) {
                soxCommand += \` sine \${frequence}\`
            } else {
                soxCommand += \` \${waveType} \${frequence}\`
            }
        })

        toCheck = \`\\\\b(?<=^([^\"]|\"[^\"]*\")*)(\${variableName})\\\\b\`;
        regMatch = line.match(toCheck)
        console.log(line.slice(0, regMatch.index), '\x1b[32m', line.slice(regMatch.index, regMatch.index + variableName.length), '\x1b[0m', line.slice(regMatch.index + variableName.length))
        execSync(soxCommand);
    }
    `
    ];

    let currentFunction = null;

    let openingBracketMatch;
    let closingBracketMatch;
    let openingBracketMatchCount = 0;
    let closingBracketMatchCount = 0;

    fileContent.split('\n').forEach((line, index) => {
        if (currentFunction !== null) {
            compiledFile = compiledFile.concat(writeSoundLines(line, fileData[currentFunction]));
        }

        currentFunction = getFunctionName(line, currentFunction);
        openingBracketMatch = line.match(/{/g)
        openingBracketMatchCount += openingBracketMatch ? openingBracketMatch.length : 0
        closingBracketMatch = line.match(/}/g)
        closingBracketMatchCount += closingBracketMatch ? closingBracketMatch.length : 0
        if (openingBracketMatchCount === closingBracketMatchCount) {
            currentFunction = null;
        }

        compiledFile.push(line);
    })

    fs.writeFileSync(DESTINATION_FILE, compiledFile.join('\n'));
    eval(compiledFile.join('\n'));
}

transcompileFile(originalFilePath);
