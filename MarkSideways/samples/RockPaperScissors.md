# Rock Paper Scissors

> As with all MarkSideways programs, this is *NOT* just a markdown file that refers to code. The markdown file itself *IS* the code that MarkSideways interprets. You can run this program by running `python ms.py samples/RockPaperScissors.md`

This is a command line version of the classic game Rock Paper Scissors. You still have to play it with your hands, it's just that your hands are now on the keyboard.


We begin our Rock Paper Scissors championship career with a friendly greeting...

```
print("**********************************");
print("* ROCK PAPER SCISSORS SHOWDOWN!! *");
print("**********************************\n\n");
```

The score will be stored in these variables. The main game loop will be in this scope, so this is fine.

```
yourScore = 0;
computerScore = 0;
```

The main game loop begins. 
```
while true {
```

A new **Game Round** instance is created that will track all the information for the current round.

```
    round = GameRound.init();
```

This will instruct the user to make some sort of fateful choice.
```
    round.makeYourPick();
```

Maybe the user decided to take the easy way out and pressed `X` for Exit. We break out of the game loop in this case.

```
    if round.isRunningAway() {
        print("You can run but you cannot hide forever.");
        break;
    }
```

For those that are brave enough to stick it out, we now SHOOT and then get the result.

```
    round.shoot();
    result = round.getResult();
```

The result is either a `TIE`, `WIN`, or `LOSE`, as a string. We announce the result and adjust the score accordingly.

```
    if result == 'TIE' {
        print("It's a TIE! D:");
    } else if result == 'WIN' {
        print("Human is VICTORIOUS");
        yourScore++;
    } else if result == 'LOSE' {
        print("Computer is TRIUMPHANT");
        computerScore++;
    } else {
        assert(false, "Something weird happened.");
    }
```
Once the score has been adjusted, we display the new score.

```
    print("Current Score:");
    print("  HUMAN:    " + yourScore);
    print("  COMPUTER: " + computerScore);
}
```

## Game Round

This just establishes the fields that are used by the class. 

```
this.humanChoice = null;
this.computerChoice = null;
```

### Make Your Pick

The human is provided a menu with a wide variety of options.

```
this.humanChoice = null;

print("--------------");
print("Choices:");
print(" R --> Rock");
print(" P --> Paper");
print(" S --> Scissors");
print(" X --> Exit");

```

We ask the human repeatedly until they make a valid choice. Was `do-while` necessary here? Perhaps not, but I spent several minutes implementing do-while so by golly, we will use it at least once.

```
do {
    choice = read_input("Your Choice: ");
    if choice == 'R' || choice == 'P' || choice == 'S' || choice == 'X' {
        this.humanChoice = choice;
    }
} while this.humanChoice == null;
```
This line makes things more readable, I guess.
```
print("--------------");
```

### Shoot!

It is now the computers turn to decide. A random number between 0 and 2 is generated.
```
n = floor(random_float() * 3);
if n == 0 {
    this.computerChoice = 'R';
} else if n == 1 {
    this.computerChoice = 'P';
} else {
    this.computerChoice = 'S';
}
```

If the computer and human chose the same thing, then it is a tie!
```
if this.computerChoice == this.humanChoice {
    this.result = 'TIE';
}
```

Otherwise, we need to go through and figure out what the result is. Basically we just build up a quick lookup table to avoid a mess of if statements.

```
else {
    configurations = {
        'R-P': false,
        'R-S': true,
        'P-R': true,
        'P-S': false,
        'S-R': false,
        'S-P': true,
    };

    this.result = configurations[this.humanChoice + '-' + this.computerChoice] ? 'WIN' : 'LOSE';
}
```

### Is Running Away?

Converts the user's choice into a boolean for whether or not they can't take the pressure.

```
return this.humanChoice == 'X';
```

### Get Result

Simple getter for the result. This isn't strictly necessary but accessing fields directly feels icky because I grew up with an abusive Java past.

```
return this.result;
```
