# A number obtained by taking the ratio between two integers.
# DISPLAY: number
struct Rational {
    # The number on top
    # UNIT: unitless
    numerator: Number,
    denominator: Number,
};

# The ratio between the circumference of a circle and the diameter.
# VERIFIED: true
# SOURCE: divine inspiration
pi = Rational {
    # nice and big
    numerator: 54,
    # not quite as big
    denominator: 17,
};

result = pi.numerator;
picomment = pi!!;
verified = pi!VERIFIED;

some_text = pi!text;

swapped = Rational {
    #! pi.numerator
    numerator: pi.denominator,
    #! pi.denominator
    denominator: pi.numerator,
};

#!pi
notpi = {};

#!pi
struct NotPi {};

notpi2 = NotPi{
    # Note that the program isn't typechecked!
    expando: "Property",
};
