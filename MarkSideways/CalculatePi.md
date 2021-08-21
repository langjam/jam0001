# Calculate Pi

> As with all MarkSideways programs, this is *NOT* just a markdown file that refers to code. The markdown file itself *IS* the code that MarkSideways interprets. You can run this program by running `python ms.py CalculatePi.md`

This program <s>calculates</s> estimates &pi; by running a simulation of picking random points within a square and checking to see if those points are within the bounds of a inscribed circle.

The ratio of points randomly chosen that fall within the circle (using the distance formula from the center of the square) should roughly correspond to the ratio of the area of the circle to the square.

This gives us the equation: `pi * r^2 / area_of_the_square = points_within_circle / total_points_tested`

If the side length of the square is `2` then the radius of the circle is `1` and the area of the square is `4`. With these values we can simplify the equation to just: `pi = 4 * points_within_circle / total_points_tested`

You can adjust the number of points to use in the simulation by passing the number of points as a command line argument.

```
if args.length == 1 {
    test_count = parse_int(args[0]);
}
```
However, if you do not pass in anything, `1000` is used as a default:
```
else {
    test_count = 1000;
}
```

We then run the simulation as a series of individual tests

```
hits = 0;
for i = 1 thru test_count {
    is_in_circle = doTest();
    if is_in_circle {
        hits++;
    }
}
```

With the number of hits within the circle known, we can perform the final estimation for the value of &pi;:

```
pi_estimate = 4.0 * hits / test_count;
print(pi_estimate);
```

### Do Test

The actual test starts by choosing a random x and y coordinate in the ranges of `(0, 0)` through `(2, 2)`

```
x = random_float() * 2;
y = random_float() * 2;
```

We find the distance from the center of this 2x2 square:

```
dist = sqrt((x - 1) ** 2 + (y - 1) ** 2);
```

If the distance is less than 1, this is within the circle!

```
return dist < 1;
```

There is no useful distinction between using `<` and `<=` since these are arbitrary float values.
