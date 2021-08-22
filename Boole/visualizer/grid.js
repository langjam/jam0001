
const TILE_SIZE = 50;
const OP_SIZE = TILE_SIZE * 3/4
const TRAIN_SPEED = .3;

const DIRECTION = {
    North: "north",
    East: "east",
    South: "south",
    West: "west",
}

const TILE_TYPE = {
    Horizontal: {name: "track_straight_EW.png", img: null, rot: DIRECTION.North},
    Vertical: {name: "track_straight_NS.png", img: null, rot: DIRECTION.North},

    NE: {name: "track_corner_NE.png", img: null, rot: DIRECTION.North},
    ES: {name: "track_corner_ES.png", img: null, rot: DIRECTION.North},
    SW: {name: "track_corner_SW.png", img: null, rot: DIRECTION.North},
    WN: {name: "track_corner_WN.png", img: null, rot: DIRECTION.North},

    T_WEST: {name: "T_crossing_left.png", img: null, rot: DIRECTION.North},
    T_NORTH: {name: "T_crossing_left.png", img: null, rot: DIRECTION.East},
    T_EAST: {name: "T_crossing_left.png", img: null, rot: DIRECTION.South},
    T_SOUTH: {name: "T_crossing_left.png", img: null, rot: DIRECTION.West},
    CROSSING: {name: "crossing.png", img: null, rot: DIRECTION.North}
}

const COLOR = {
    LightRed: "lightred",
    DarkBlue: "darkblue",
    DarkRed: "darkred",
    LightBlue: "lightblue",
    DarkGreen: "darkgreen",
    LightGreen: "lightgreen",
    WaterBlue: "waterblue",
    Brown: "brown",
    Yellow: "yellow",
    Orange: "orange",
}

const STATION_TYPE = {
    "delete": "op_delete.png",
    "duplicate": "op_duplicate.png",
    "nothing": "op_nothing.png",
    "rotate": "op_rotate.png",
    "transfer": "op_transfer.png",
    "delete_top": "op_delete_top.png",

    "input": "op_input.png",
    "print_number": "op_print_number.png",
    "print_string": "op_print_string.png",

    "add": "op_add.png",
    "sub": "op_sub.png",
    "mul": "op_mul.png",
    "mod": "op_mod.png",
    "div": "op_div.png",

    "switch_eqz": "op_switch%20eq.png",
    "switch_gtez": "op_switch%20gte.png",
    "switch_empty": "op_switch%20empty.png",
}

let locomotiveBackground;
let locomotiveForeground;
let locomotiveAccent = {}
let locomotiveAccent1 = {}
let stationForeground;
let stationBackground;
let stopper;
let cloud;
let stationTypeImages = {}
let lineLookup = new Map();



function preloadTrain() {
    locomotiveBackground = loadImage("tiles/locomotive_background.png");
    locomotiveForeground = loadImage("tiles/locomotive_foreground.png");

    for (const i in COLOR) {
        const color = COLOR[i];
        locomotiveAccent[color] = loadImage(`tiles/locomotive_accent_${color}.png`)
        locomotiveAccent1[color] = loadImage(`tiles/locomotive_accent1_${color}.png`)
    }

    stationBackground = loadImage("tiles/station_bottom.png")
    stationForeground = loadImage("tiles/station_top.png")
    stopper = loadImage("tiles/stopper.png")
    cloud = loadImage("tiles/cloud.png");

    for (const i in STATION_TYPE) {
        const station = STATION_TYPE[i];
        stationTypeImages[i] = loadImage(`tiles/${station}`)
    }
}

function drawTile(x, y, w, h, tile_type) {
    image(tile_type.img, x, y, w, h);
}

class Grid {
    grid
    trains
    stations

    constructor() {
        this.grid = new Map();
        this.trains = new Map();
        this.stations = new Map();
    }


    addTile(coordinate, tile) {
        this.grid.set(coordinate, {tile: tile, rotation: tile.rot})
    }

    addTrain(train) {
        this.trains.set(train.identifier, train)
    }

    addStation(station) {
        this.stations.set(station.name, station)
    }

    draw() {

        for (const i of this.stations) {
            i[1].drawBackground()
        }

        for (const i of this.grid) {
            const location = i[0];
            const tile = i[1];


            push()
            translate(location.x * TILE_SIZE, location.y * TILE_SIZE)

            translate(TILE_SIZE / 2, TILE_SIZE / 2);

            switch (tile.rotation) {
                case DIRECTION.North: break;
                case DIRECTION.East: rotate(radians(90));  break;
                case DIRECTION.South: rotate(radians(180)); break;
                case DIRECTION.West: rotate(radians(-90)); break;
            }

            translate(-TILE_SIZE / 2, -TILE_SIZE / 2);


            drawTile(0, 0, TILE_SIZE, TILE_SIZE, tile.tile)
            pop()
        }

        for (const i of this.trains) {
            i[1].draw()
        }


        for (const i of this.stations) {
            i[1].drawForeground()
        }
    }

    hasTrain(train_identifier) {
        return this.trains.has(train_identifier)
    }
}

function getRandom(min, max) {
    return Math.random() * (max - min) + min;
}


class Cloud {
    location
    size
    fadespeed
    trajectory
    opacity

    constructor(location) {
        this.location = location;

        this.size = getRandom(0.5, 0.8);
        const trajx = getRandom(-2, 2);
        const trajy = getRandom(-2, 2);
        this.fadespeed = getRandom(0.1, 0.2);

        this.trajectory = createVector(trajx, trajy);
        this.opacity = 0.7;
    }

    draw() {
        this.location.x += this.trajectory.x * TRAIN_SPEED;
        this.location.y += this.trajectory.y * TRAIN_SPEED;
        this.opacity -= this.fadespeed * TRAIN_SPEED;
        if (this.opacity <= 0) {
            return false;
        }
        push()
        translate(this.location.x, this.location.y);
        tint(255, this.opacity * 255);
        image(cloud, 0, 0, TILE_SIZE * this.size, TILE_SIZE * this.size);
        pop()
        return true;
    }
}

class Train {
    location
    accent
    accent1
    direction
    identifier

    path
    animation_count
    path_index
    traveling_to
    traveling_from

    constructor(location, accent, accent1, direction, identifier) {
        this.location = location;
        this.accent = accent;
        this.accent1 = accent1;
        this.direction = direction;
        this.path = null;
        this.identifier = identifier;
        this.animation_count = 0;
        this.traveling_to = null;
        this.traveling_from = null;

        this.clouds = []
        this.ticks_since_last_cloud = 0;
    }

    travelAlongPath(path) {
        this.path = path
        this.animation_count = 0;
        this.path_index = 0
        if (path.length > 1) {
            this.traveling_to = this.path[this.path_index + 1];
            this.traveling_from = this.path[this.path_index];
        } else {
            // if path length is 1, don't bother
            this.path = null;
        }
    }

    update() {
        if (this.animation_count > 1) {
            this.animation_count = 0
            this.path_index += 1;

            if (this.path_index + 1 > this.path.length - 1) {
                this.path = null;
                return
            } else {
                this.traveling_to = this.path[this.path_index + 1]
                this.traveling_from = this.path[this.path_index]
            }
        }

        if (this.traveling_to[0] > this.traveling_from[0]) {
            this.direction = DIRECTION.East;
        } else if (this.traveling_to[0] < this.traveling_from[0]) {
            this.direction = DIRECTION.West;
        }else if (this.traveling_to[1] > this.traveling_from[1]) {
            this.direction = DIRECTION.South;
        }else {
            this.direction = DIRECTION.North;
        }

        const x = lerp(this.traveling_from[0], this.traveling_to[0], this.animation_count)
        const y = lerp(this.traveling_from[1], this.traveling_to[1], this.animation_count)

        // console.log(x, y, this.traveling_from, this.traveling_to, this.animation_count)

        this.location.x = x;
        this.location.y = y;

        this.animation_count += TRAIN_SPEED;
    }

    draw() {
        if (this.path !== null) {
            this.update()
        }

        push()
        translate(this.location.x * TILE_SIZE, this.location.y * TILE_SIZE)

        translate(TILE_SIZE / 2, TILE_SIZE / 2);
        switch (this.direction) {
            case DIRECTION.North: rotate(radians(-90)); break;
            case DIRECTION.East: break;
            case DIRECTION.South: rotate(radians(90)); break;
            case DIRECTION.West: rotate(radians(180)); break;
        }
        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);


        image(locomotiveBackground, 0, 0, TILE_SIZE, TILE_SIZE)
        image(locomotiveAccent[this.accent], 0, 0, TILE_SIZE, TILE_SIZE)
        image(locomotiveAccent1[this.accent1], 0, 0, TILE_SIZE, TILE_SIZE)
        image(locomotiveForeground, 0, 0, TILE_SIZE, TILE_SIZE)
        pop()

        push()
        if (this.path !== null) {
            this.ticks_since_last_cloud += 1;
            if (this.ticks_since_last_cloud > 2) {
                this.ticks_since_last_cloud = 0;
                this.clouds.push(new Cloud(createVector(this.location.x * TILE_SIZE, this.location.y * TILE_SIZE)));
            }
        }
        const new_clouds = []
        for (const cloud of this.clouds) {
            if (cloud.draw()) {
                new_clouds.push(cloud);
            }
        }
        this.clouds = new_clouds;
        pop()
    }
}

class Station {
    location
    stopped
    station_type
    name

    constructor(location, station_type, name, stopped = []) {
        this.location = location;
        this.stopped = stopped;
        this.station_type = station_type;
        this.name = name;
        this.last_print = "";
    }

    drawBackground() {
        push()
        translate(this.location.x * TILE_SIZE, this.location.y * TILE_SIZE);
        image(stationBackground, 0, 0, TILE_SIZE * 2, TILE_SIZE * 2)
        pop()
    }

    drawForeground() {
        push()
        translate(this.location.x * TILE_SIZE, this.location.y * TILE_SIZE);
        image(stationForeground, 0, 0, TILE_SIZE * 2, TILE_SIZE * 2)

        for (let i = 0; i < 8; i++) {
            if (i < this.stopped.length && !this.stopped[i]) {
                push()
                switch (i) {
                    case 5:
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break;
                    case 4:
                        translate(TILE_SIZE, 0);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break;
                    case 3:
                        translate(TILE_SIZE, 0);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(90));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break;
                    case 2:
                        translate(TILE_SIZE, 0);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(90));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        translate(TILE_SIZE, 0);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break;
                    case 1:
                        translate(TILE_SIZE, TILE_SIZE);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(180));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break;
                    case 0:
                        translate(TILE_SIZE, TILE_SIZE);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(180));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        translate(TILE_SIZE, 0);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break
                    case 7:
                        translate(0, TILE_SIZE);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(-90));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break
                    case 6:
                        translate(0, TILE_SIZE);
                        translate(TILE_SIZE / 2, TILE_SIZE / 2);
                        rotate(radians(-90));
                        translate(-TILE_SIZE / 2, -TILE_SIZE / 2);
                        translate(TILE_SIZE, 0);
                        image(stopper, 0, 0, TILE_SIZE, TILE_SIZE);
                        break
                }
                pop()
            }
        }

        push()
        translate(TILE_SIZE * 5/8, TILE_SIZE * 5/8);
        image(stationTypeImages[this.station_type], 0, 0, OP_SIZE, OP_SIZE)
        pop()

        push()
        translate(TILE_SIZE, -TILE_SIZE * (1/8));
        fill(255)
        stroke(255)
        textAlign(CENTER, CENTER);
        text(this.name, 0, 0)
        pop()

        if (this.last_print !== "") {
            push();
            translate(TILE_SIZE, -TILE_SIZE * (5 / 8));
            fill(255);
            stroke(255);
            textAlign(CENTER, CENTER);
            textSize(18);
            text(this.last_print, 0, 0);
            pop();
        }

        pop()
    }
}