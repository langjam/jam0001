/*
 *  Tic-Tac-Toe
 *  Translated from https://rosettacode.org/wiki/Tic-tac-toe#Python
 */;

let board = list(1, 2, 3, 4, 5, 6, 7, 8, 9);

let wins = list(
    list(0, 1, 2),
    list(3, 4, 5),
    list(6, 7, 8),
    list(0, 3, 6),
    list(1, 4, 7),
    list(2, 5, 8),
    list(0, 4, 8),
    list(2, 4, 6));

let print_board = fn() {
    print(get(board, 0), get(board, 1), get(board, 2));
    print(get(board, 3), get(board, 4), get(board, 5));
    print(get(board, 6), get(board, 7), get(board, 8));
};

let score = fn() {
   let i = 0;
   let not_done = True;
   while (i < len(wins) & not_done) {
       let w = get(wins, i);
       i = i + 1;
       let c = get(board, get(w, 0));
       if (c == get(board, get(w, 1)) & c == get(board, get(w, 2))) {
           not_done = False;
           True
       } else {
           False
       };
   }
};

let finished = fn() {
    let i = 0;
    let marks = 0;
    while (i < len(board)) {
        let c = get(board, i);
        if (c == "X" | c == "O") {
            marks = marks + 1;
        };
        i = i + 1;
    };
    marks == 9
};

let spaces = fn() {
    let spaces = list();
    let i = 0;
    while (i < len(board)) {
        let c = get(board, i);
        if (c == "X" | c == "O") {
        } else {
            append(spaces, i)
        };
        i = i + 1;
    };
    spaces /* the list of the free spaces on the board */
};

let my_turn = fn() {
    let options = spaces();
    let choice = get(options, randint(0, len(options) - 1));
    print("I go at index " + str(choice + 1));
    put(board, choice, "O");
};

let your_turn = fn() {
    let not_done = True;
    while (not_done) {
        print("Your turn. Input the index of where you wish to place your mark" /* prompt */);
        let choice = num(input());
        if (choice == ()) {
            print("sorry I did not undestand that" /* error message */);
        } else if (choice < 1 | choice > 9) {
            print("that is not on the board" /* error message */);
        } else if (get(board, choice - 1) == "X" | get(board, choice - 1) == "O") {
            print("that spot is already taken" /* error message */);
        } else {
            put(board, choice - 1, "X");
            not_done = False;
        };
    };
};
        
let not_done = True;
print_board();
while (not_done) {
    your_turn();
    print_board();
    if (score()) {
        print("A strange game. The only winning move is not to play." /* a cool movie reference */);
        not_done = False;
    } else if (finished()) {
        print("It's a draw!");
        not_done = False;
    } else {
        my_turn();
        print_board();
        if (score()) {
            print("I win!" /* victory message */);
            not_done = False;
        };
    };
};