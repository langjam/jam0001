
const zoomSpeed = .2;
const minimumZoom = .3;
const default_scale = 10;
const controlsHeight = 40;

async function ask_for_input() {
    return prompt("input")
}

let socket;
window.addEventListener("load", () => {
    socket = startSocket()
})

let w = window.innerWidth;
let h = window.innerHeight - controlsHeight;
let canvas;

class Station {
    id
    x
    y

    constructor(id, x, y) {
        this.id = id;
        this.x = x;
        this.y = y;
    }
}

const stations = [];

const world = {
    x: 0,
    y: 0,
    zoom: 1,

    px: 0,
    py: 0,
}

function centerZero() {
    world.x = (w / 2);
    world.y = (h / 2);
    world.zoom = 1;
}

function setup() {
    canvas = createCanvas(w, h)

    stations.push(new Station(1, 10, 10));
    stations.push(new Station(1, 20, 10));
    stations.push(new Station(1, 10, 20));

    document.getElementById("controls").style.height = `${controlsHeight}px`;
    centerZero();
}

function draw() {
    background(51);

    push()

    translate(world.x, world.y);
    scale(world.zoom)

    for (const station of stations) {
        circle(station.x, station.y, 10);
    }

    pop()
}

function mouseWheel(event) {
    const {x, y, deltaY} = event;
    const direction = deltaY > 0 ? -1 : 1;
    const zoom = 1 * direction * zoomSpeed;

    // compute the weights for x and y
    const wx = (x-world.x)/(w*world.zoom);
    const wy = (y-world.y)/(h*world.zoom);

    // apply the change in x,y and zoom.
    if (world.zoom + zoom >= minimumZoom) {
        world.x -= wx*w*zoom;
        world.y -= wy*h*zoom;

        world.zoom += zoom;
    }

    if (world.zoom < minimumZoom) {
        world.zoom = minimumZoom;
    }
}

function mousePressed() {
    world.px = mouseX;
    world.py = mouseY;

    document.getElementById("body").style.cursor = "pointer";
}

function mouseReleased() {
    document.getElementById("body").style.cursor = "unset";
}

function mouseDragged(event) {
    const {px, py} = world;

    const pos = {x: event.clientX, y: event.clientY};
    const dx = pos.x - px;
    const dy = pos.y - py;

    if(dx || dy) {
        world.x += dx;
        world.y += dy;
        world.px = pos.x;
        world.py = pos.y;
    }
}

window.onresize = function() {
    w = window.innerWidth;
    h = window.innerHeight - controlsHeight;
    resizeCanvas(w, h);

    document.getElementById("controls").style.height = `${controlsHeight}px`;
}