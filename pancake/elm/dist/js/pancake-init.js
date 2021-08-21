/* Util. */
function compile(data) {
  src.send(data);
}

function onPulse(pulse) {
  console.log("got pulse:", pulse);
}

/* Elm stuff. */
const app = Elm.Main.init();

/* Ports. */
const src = app.ports.src;
const commd = app.ports.commd;
const event = app.ports.event;

/* Subscriptions. */
event.subscribe(onPulse);
