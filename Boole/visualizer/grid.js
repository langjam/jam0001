
const TILE_SIZE = 50;
const OP_SIZE = TILE_SIZE * 3/4

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
        this.grid = new Map()
        this.trains = []
        this.stations = []
    }

    addTile(coordinate, tile, rotation=DIRECTION.North) {
        this.grid.set(coordinate, {tile: tile, rotation: rotation})
    }

    addTrain(train) {
        this.trains.push(train)
    }

    addStation(station) {
        this.stations.push(station)
    }

    draw() {

        for (const i of this.stations) {
            i.drawBackground()
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
            i.draw()
        }


        for (const i of this.stations) {
            i.drawForeground()
        }
    }
}

class Train {
    location
    accent
    accent1
    direction

    constructor(location, accent, accent1, direction) {
        this.location = location;
        this.accent = accent;
        this.accent1 = accent1;
        this.direction = direction;
    }

    draw() {
        push()


        translate(this.location.x * TILE_SIZE, this.location.y * TILE_SIZE)
        switch (this.direction) {
            case DIRECTION.North: rotate(radians(-90)); break;
            case DIRECTION.East: break;
            case DIRECTION.South: rotate(radians(90)); break;
            case DIRECTION.West: rotate(radians(180)); break;
        }

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

    constructor(location, station_type, stopped = []) {
        this.location = location;
        this.stopped = stopped;
        this.station_type = station_type;
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

        translate(TILE_SIZE * 5/8, TILE_SIZE * 5/8);
        image(stationTypeImages[this.station_type], 0, 0, OP_SIZE, OP_SIZE)

        pop()
    }
}