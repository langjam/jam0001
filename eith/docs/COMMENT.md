# Comment

### IS-COMMENT

( n1 -- n2 )

Checks if line `n1` is a comment or not. Pushes -1 onto the stack if
it is and 0 otherwise.

### IS-COMMENT-RANGE

( n1 n2 -- n3 )

Checks if all lines in range `[n1, n2)` are comments. Pushes -1 onto the stack if
so and 0 otherwise.

### READ-COMMENT

( n -- )

Pushes commented integer on line `n` onto the stack.


### COMMENT

( n -- )
Comments line `n`.


### UNCOMMENT

( n -- )

Uncomments line `n`.

### TOGGLE-COMMENT

( n -- )

Toggles comment on line `n`.

### COMMENT-RANGE

( n1 n2 -- )

Comments lines in range `[n1, n2)`.


### UNCOMMENT-RANGE

( n1 n2 -- )

Uncomments lines in range `[n1, n2)`.

### TOGGLE-COMMENT-RANGE

( n1 n2 -- )

Toggles comments on lines in range `[n1, n2)`