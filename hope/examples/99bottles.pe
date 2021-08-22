/* when the body of a function is a single expression, the braces can be ommited */;
let bottles = fn(n)
    if (n == 0)
        "no more bottles"
    else if (n == 1)
        "one bottle"
    else
        str(n) + " bottles";

let print_verse = fn(i) {
    let n = 99 - i;
    print(bottles(n) + " of beer on the wall"         {/* the first line of verse number */ + cmnt(i)});
    print(bottles(n) + " of beer"                     {/* the second line of verse number */ + cmnt(i)});
    if (n > 0) {
        print("take one down, pass it around"         {/* the third line of verse number */ + cmnt(i)});
        print(bottles(n - 1) + " of beer on the wall" {/* the fouth line of verse number */ + cmnt(i)});
   };
   print();
};

let i = 0;
while (i < 99) {
    print_verse(i);
    i = i + 1;
};
