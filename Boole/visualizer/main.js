
const zoomSpeed = .1;
const minimumZoom = .1;
const default_scale = 10;
const controlsHeight = 40;

async function ask_for_input(prompt_message) {
    return prompt(prompt_message)
}

let paused = false;
function togglePause() {
    paused = !paused;
    if (paused) {
        document.getElementById("pause").style.background = "orangered";
    } else {
        document.getElementById("pause").style.background = "unset";
    }
}

let continueSimulation = false;
function toggleContinue() {
    continueSimulation = !continueSimulation;
    if (continueSimulation) {
        document.getElementById("continue").style.background = "green";

        if (stopcount === grid.trains.size) {
            socket.nextTimeStep()
        }
    } else {
        document.getElementById("continue").style.background = "unset";
    }
}

let socket;
window.addEventListener("load", () => {
    socket = startSocket()

    const current_error = window.localStorage.getItem("error");
    if (current_error !== null && typeof current_error !== "undefined") {
        error(current_error);
        window.localStorage.removeItem("error");
    }
})

let w = window.innerWidth;
let h = window.innerHeight - controlsHeight;
let canvas;
let grid = new Grid();

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

function preload() {
    for (const tiletypeKey in TILE_TYPE) {
        TILE_TYPE[tiletypeKey].img = loadImage("tiles/" + TILE_TYPE[tiletypeKey].name)
    }

    preloadTrain()
}

function loadData(path) {
    fetch(path, {
        method: "GET"
    }).then((r) => {
        r.json().then((j) => {
            console.log(j)
            const stations = j["stations"];
            const lines = j["lines"];
            const tiles = j["tiles"];

            for(const station of stations) {
                grid.addStation(new Station(
                    createVector(station["x"], station["y"]), station["type"], station["name"], station["stoppers"],
                ))
            }

            for(const tile of tiles) {
                grid.addTile(
                    createVector(tile["x"], tile["y"]), TILE_TYPE[tile["type"]],
                )
            }

            for(const line of lines) {
                const station_id = line["station_id"];
                const station_name = stations[station_id]["name"];
                const station_track = line["station_track"];
                const path = line["path"];

                let s;
                if (lineLookup.has(station_name)) {
                    s = lineLookup.get(station_name)
                } else {
                    s = new Map()
                }

                s.set(station_track, path)
                lineLookup.set(station_name, s)
            }

            document.getElementById("loading").style.display = "none";
            updateFinishedValue();
        })
    })

}

function setup() {
    canvas = createCanvas(w, h)

    document.getElementById("controls").style.height = `${controlsHeight}px`;
    //
    // grid.addTile(createVector(5, 5), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(4, 5), TILE_TYPE.NE)
    // grid.addTile(createVector(4, 4), TILE_TYPE.Vertical)
    // grid.addTile(createVector(4, 3), TILE_TYPE.ES)
    // grid.addTile(createVector(5, 3), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(6, 3), TILE_TYPE.T_LEFT)
    // grid.addTile(createVector(6, 4), TILE_TYPE.Vertical)
    // grid.addTile(createVector(6, 5), TILE_TYPE.WN)
    //
    // grid.addTile(createVector(6, 2), TILE_TYPE.Vertical)
    // grid.addTile(createVector(6, 1), TILE_TYPE.SW)
    // grid.addTile(createVector(5, 1), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(4, 1), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(3, 1), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(2, 1), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(2, 0), TILE_TYPE.Horizontal)
    // grid.addTile(createVector(0, -1), TILE_TYPE.Vertical)
    //
    // grid.addStation(new Station(createVector(0, 0), [false, true, false, true, true, false, false, true], STATION_TYPE.Add))

    // all train colors
    // let x = 0;
    // let y = 0;
    // for (const i in COLOR) {
    //     x = 0;
    //     y += 1;
    //     for (const j in COLOR) {
    //         x += 1;
    //         grid.addTrain(new Train(createVector(x, y), COLOR[i], COLOR[j], DIRECTION.East))
    //     }
    // }

    centerZero();
}

prints = []
function draw() {
    background(0x56, 0x7d, 0x46);

    push()

    translate(world.x, world.y);
    scale(world.zoom)

    grid.draw()

    pop()


    push()
    translate(10, 10);
    for (const m of prints) {
        translate(0, 25);
        push();
        fill(255);
        stroke(255);
        textAlign(LEFT, CENTER);
        textSize(23);
        text(m, 0, 0);
        pop();
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

    event.preventDefault();
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
    if (mouseY<0){
        return
    }
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

let error_timeout = null;
function error(message) {
    const error_elem = document.getElementById("error");
    error_elem.innerText = message;

    if (error_timeout !== null) {
        clearTimeout(error_timeout);
    }

    error_elem.style.visibility = "visible";
    error_timeout = setTimeout(() => {
        error_elem.style.visibility = "hidden";
    }, 10 * 1000)
}
