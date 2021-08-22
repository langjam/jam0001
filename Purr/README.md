# Purr
Purr is composition of Cat and Joy (in a conCATenative sense).

In the same way words are composed, comments will also be composable as a noun or verb, wow really, can't wait to see that :).

## Build
This repo includes a 'dist' folder that contains compiled Javascript (from the Typescript source) 

'nodejs' is required run, so if you don't have 'node', then this is for you -- install at (nodejs.org)[nodejs]

### Dev setup
Purr is written in Typescript so to develope/contribute...
> npm install typescript --save-dev

Check your version
> npx tsc --version

best if Version 4.3.5 (: or above :)
also we need command line args from node , so
> npm i --save-dev @types/node



## Run
Time to purr. To run a program...
> node ./dist/purr.js '2 3 +' 

## Tests
You can run *tests* with out setting up the dev environment (aka Typescript compiler)
> npm run tests-with-no-compile

If you want to try changing any tests, you will need the Dev environment setup and then run
> npm test

