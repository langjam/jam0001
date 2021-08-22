# Builtin Functions

|       **Name**      |              **Prototype**              |                                                    **Description**                                                    |
| :-----------------: | :-------------------------------------: | :-------------------------------------------------------------------------------------------------------------------: |
|        print        |              Any\* -> Void              |                                                Prints values to stdout                                                |
|       println       |              Any\* -> Void              |                                     Prints values to stdout, followed by a newline                                    |
|      read line      |              Void -> String             |                                         Reads a line from stdin into a string                                         |
|      length of      |       (String \| Array) -> Integer      |                                        Returns the length of a string or array                                        |
|         pow         |       Integer, Integer -> Integer       | Raises the first argument to the power of the second. The second argument must be nonnegative and less than u32::MAX. |
|     split string    |     String, String -> Array\<String\>   |          Splits the first argument using the second argument as the delimiter, returning an array of strings.         |
| floating point cast |  (Integer \| Float \| String) -> Float  |                        Casts a value to a float, by converting an integer or parsing a string.                        |
|     integer cast    | (Integer \| Float \| String) -> Integer |                        Casts a value to an integer, by truncating a float or parsing a string.                        |
|    random integer   |             Void -> Integer             |                                  Returns a random integer from i64::MIN to i64::MAX.                                  |
|  get char in string |        String, Integer -> String        |                                      Gets a character from a string by its index.                                     |