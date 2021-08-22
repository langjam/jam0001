# An example demonstrating uncertainty analysis.
_ = ();

# The basic assumptions of our model.
assumptions = {
    # How many people live in Berlin?
    # UNCERTAINTY: -20%/+30%
    population: 54321,

    # How many people are cat owners?
    # UNCERTAINTY: -50%/+50%
    cat_owners: 34567,

    # How many cats does each cat owner own?
    # UNCERTAINTY: -1/+5
    cats_per_owner: 2,
};

# The model code would calculate values.
# The uncertainty module would propagate for uncertainty values.
results = {
    # The total number of cats in Berlin.
    # UNCERTAINTY: -2%/+1%
    total_cats: assumptions.population + assumptions.cat_owners + assumptions.cats_per_owner,
};
