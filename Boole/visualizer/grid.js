
const TILE_SIZE = 50;
const OP_SIZE = TILE_SIZE * 3/4
const TRAIN_SPEED = .3;

const TILE_TYPE = {
    Horizontal: {name: "track_straight_EW.png", img: null},
    Vertical: {name: "track_straight_NS.png", img: null},

    NE: {name: "track_corner_NE.png", img: null},
    ES: {name: "track_corner_ES.png", img: null},
    SW: {name: "track_corner_SW.png", img: null},
    WN: {name: "track_corner_WN.png", img: null},

    T_LEFT: {name: "T_crossing_left.png", img: null},
    CROSSING: {name: "crossing.png", img: null}
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

const DIRECTION = {
    North: "north",
    East: "east",
    South: "south",
    West: "west",
}

let locomotiveBackground;
let locomotiveForeground;
let locomotiveAccent = {}
let locomotiveAccent1 = {}
let stationForeground;
let stationBackground;
let stopper;
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


    addTile(coordinate, tile, rotation=DIRECTION.North) {
        this.grid.set(coordinate, {tile: tile, rotation: rotation})
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

        console.log(x, y, this.traveling_from, this.traveling_to, this.animation_count)

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

        pop()
    }
}