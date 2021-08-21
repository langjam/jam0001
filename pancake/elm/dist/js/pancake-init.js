/* Util. */
function compile(data) {
  compiler.send(data)
}

function step() {
  steper.send(true)
}

function onResult(compilationResult) {
  console.log("compilation result:", compilationResult)
}

function onState(status) {
  console.log("status:", status)
}

/* Elm stuff. */
const app = Elm.Main.init()

/* Ports. */
const compiler = app.ports.compile // ->
const result = app.ports.result // <-
const steper = app.ports.step // ->
const state = app.ports.state // <-

/* Subscriptions. */
result.subscribe(onResult)
state.subscribe(onState)
