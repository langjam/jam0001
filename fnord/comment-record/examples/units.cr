# An example demonstrating units of measure.
_ = ();

struct Inputs {
    # The length of time we will be travelling.
    # UNIT: fortnight
    time: Number,

    # The distance we need to travel.
    # UNIT: furlong
    distance: Number,
};

struct Outputs {
    # The speed we need to travel.
    # UNIT: furlongs per fortnight
    speed: Number,
};

result = Outputs {
    # n.b. in a future version this could be calculated
    speed: 42,
};
