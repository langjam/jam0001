# An example demonstrating formatting control
_ = ();

struct Data {
    # The length of time the experiment runs for.
    # UNIT: second
    # DIGITS: 2
    time: Number,

    # The number of parallel servers.
    # DIGITS: 0
    servers: Number,

    # The amount of load to place on the server.
    # UNIT: mb/s
    # DIGITS: 1
    load: Number,

    # The file offset for the corruption.
    # UNIT: byte
    # FORMAT: hex
    offset: Number,

    # The file permissions to use.
    # FORMAT: octal
    permissions: Number,
};

short_test = Data {
    time: 10,
    servers: 20,
    load: 100,
    offset: 20,
    permissions: 744,
};

long_test = Data {
    # DIGITS: 0
    time: 60,
    servers: 3,
    load: 10,
    offset: 0,
    permissions: 744,
};
