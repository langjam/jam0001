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

inputs = Inputs {
    time: 42,
    distance: 3,
};

outputs = Outputs {
    # n.b. in a future version unit for this could be calculated
    speed: inputs.time + inputs.distance,
};

output_comment = outputs.speed!!;
result = "Result: " + outputs.speed + " " + output_comment.UNIT;
