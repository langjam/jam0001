import { stripIndent } from 'common-tags';

export type ExampleHeading = {
  heading: string;
  jumbo?: boolean;
};

export type Example = {
  name: string;
  source: string;
  repl?: Array<string>;
};

export const Examples: Array<Example | ExampleHeading> = [
  { heading: 'Yack', jumbo: true },
  {
    name: 'Welcome',
    source: stripIndent`
      # Welcome to the Yack interative walkthrough! This is the
      # editor pane, where editable example code will be shown.
      #
      # Navigate using the sidebar to the left for a walkthrough
      # of the Yack language.
    `,
    repl: [
      stripIndent`
        # This pane is the console, where you can interactively
        # play with the Yack code you write in the editor pane.
      `,
      '2 + 2',
    ],
  },
  {
    name: 'Getting Started',
    source: stripIndent`
      # You can make any changes you want here in the editor pane,
      # and they'll be reflected in any interactions you have in the
      # console on the right.

      # Here's an example of defining a top-level value in Yack:
      def value = 123

      # If you look in the console, you'll see that entering \`value\`
      # prints \`123\`. Of course, since the prompt for this jam was
      # "first-class comments", comments are a bit special in Yack.

      ###
      This a doc comment for the value declared below it. This comment
      is itself a first-class value in the Yack language.

      You can access this comment as an expression in source or
      in the console by adding a "#" at the end of its associated
      identifier, like so: {documentedValue#}.
      ###
      def documentedValue = true


      # Before going deeper on what we can do with comment values
      # in Yack, let's explore some of the language's more mundane
      # features first.
    `,
    repl: [
      stripIndent`
        # Here you can enter one-off expressions and definitions
        # using the values defined to the left. You'll then see
        # the evaluated result.
        #
        # For example:
      `,
      'value',
      '# Doc comments are themselves first class values in Yack:',
      'documentedValue#',
    ],
  },
  {
    heading: 'Language Features',
  },
  {
    name: 'Data Structures',
    source: stripIndent`
      # Given it was hacked together in 48 hours, Yack doesn't
      # have an enormous number of fun features. Instead, we
      # get the bare minimum necessary to express some interesting
      # things, even if not in the most ergonomic way.

      # The core of our toolkit is algebraic data types, declared
      # with the \`data\` keyword.

      ###
      Our go-to example ADT is a List.

      A List consists of either a {Nil} value, representing the
      empty list, or a {Cons} value, which represents a list
      element tacked on to the rest of the list.
      ###
      data List = Nil() | Cons(head, tail)

      ### An empty list is just {Nil()} ###
      def empty = Nil()

      ### We can define a list [1, 2, 3] by chaining Cons calls. ###
      def oneTwoThree = Cons(1, Cons(2, Cons(3, Nil())))
    `,
    repl: [
      'List#',
      'oneTwoThree',
      stripIndent`
        # Notice that, since doc comments are themselves first class
        # values, we can embed them in other data structures.
      `,
      'Cons(oneTwoThree#, Nil())',
    ],
  },
  {
    name: 'Pattern Matching',
    source: stripIndent`
      # Once we've built a data structure, we need a way to get
      # that information back out! Yack's answer to that is the
      # \`match\` expression.

      # Let's recreate our List example again.
      data List = Nil() | Cons(head, tail)

      # And build a 1, 2, 3 list...
      def oneTwoThree = Cons(1, Cons(2, Cons(3, Nil())))

      # And now we can turn around and get elements back out
      # by matching on the structure of the data.
      def two = match oneTwoThree
      | Cons(_, Cons(el, _)) -> el
      | _ -> void
      end
    `,
    repl: [
      stripIndent`
        # And we can confirm over here that \`two\` is in fact \`2\`.
        two
      `,
      stripIndent`
        # Note that since Yack is dynamically typed, there is
        # no requirement that your matches be exhaustive. If a
        # value is tested and none of the patterns apply, a
        # runtime error will result instead.

        match oneTwoThree
        | Nil() -> true
        end
      `,
    ],
  },
  {
    name: 'Functions',
    source: stripIndent`
      ###
      Yack has functions as first class values, with none
      of that fancy shorthand syntax for declaring them.

      This function adds {2} and {2} together to get {2 + 2}.
      ###
      def addTwoAndTwo = fun()
        2 + 2
      end
    `,
    repl: [
      stripIndent`
        # We can call it to get our answer:
        addTwoAndTwo()
      `,
      stripIndent`
        # Or if we log it directly to the console, we'll
        # see its source:
        addTwoAndTwo
      `,
      '',
      stripIndent`
        # And as with other declarations, appending a '#'
        # will show us the associated documentation.
        addTwoAndTwo#
      `,
    ],
  },
  {
    heading: 'Doc Comments',
  },
  {
    name: 'Named Examples',
    source: stripIndent`
      # Doc comments in Yack have a built-in notion of
      # named expressions called "examples".

      ###
      This is the same list structure we saw before,
      but now with some examples.

      Here's an empty list:
      @example#emptyList
        Nil()
      @end

      And here's one with some arithmetic in it:
      @example#mathList
        Cons(2 + 2,
          Cons(3 % 2,
            Nil()))
      @end
      ###
      data List = Nil() | Cons(head, tail)
    `,
    repl: [
      'List#',
      stripIndent`
        # These expressions are accessible anywhere else in
        # your code using the format \`identifier#exampleName\`.
      `,
      'List#mathList',
      '# You can also click any example to log its value to the console.',
    ],
  },
  {
    name: 'Expression Interpolation',
    source: stripIndent`
      ###
      If you've been paying close attention, you may have
      already noticed that doc comments can have arbitrary
      expressions embedded in them, even outside of examples.

      For example: {2} + {2} = {2 + 2}.

      While this has some fun uses on its own, it combines
      particularly nicely with the fact that comment values,
      when embedded in a data structure (whether it's a List
      or another comment!), render as links to their content.

      See, for instance, {theOtherExample#}.
      ###
      def forExample = void

      ###
      Hopefully you got here by clicking {theOtherExample#}
      in the console output above :)

      Embedded expressions even reference examples from
      their own containing comment!
      @example#equalityCheck
        3 == 4
      @end
      End result? {theOtherExample#equalityCheck}
      ###
      def theOtherExample = void
    `,
    repl: ['forExample#'],
  },

  {
    heading: 'Toys',
  },
  {
    name: 'FizzBuzz',
    source: stripIndent`
          ###
          We don't have strings in Yack, so instance we'll
          build a nice dummy data structure for our output.
          ###
          data FizzBuzz = Fizz() | Buzz() | FizzBuzz()

          ###
          We also want tuples, but 48 hours is not Tuple Time,
          so we improvise.
          ###
          data Pair = Pair(a, b)

          ###
          This example turned out not to be super exciting
          from a doc comments perspective. ¯\\_(ツ)_/¯
          ###
          def fizzbuzz = fun(n)
            match Pair(n % 5, n % 3)
            | Pair(0, 0) -> log(FizzBuzz())
            | Pair(0, _) -> log(Fizz())
            | Pair(_, 0) -> log(Buzz())
            | _ -> log(n)
            end;

            match n
            | 0 -> void
            | _ -> fizzbuzz(n - 1)
            end
          end
        `,
    repl: ['fizzbuzz(15)'],
  },
  {
    name: 'List Functions',
    source: stripIndent`
      # Ok, that's the whirlwind tour of the language. This last
      # section just contains a smattering of additional examples
      # that demonstrate how you might (not-super-ergonomically)
      # implement some common simple algorithms in Yack.

      ###
      A list represents an ordered collection of items.

      Since Yack is totally dynamically typed, lists can be pretty
      free-form. They don't need to contain a consistent type of
      element, and they don't even necessarily need a terminating
      {Nil()} value (though that's still recommended...)

      @example#empty
        Nil()
      @end

      @example#oneTwoThree
        Cons(1, Cons(2, Cons(3, Nil())))
      @end
      ###
      data List = Nil() | Cons(head, tail)

      ###
      This returns the first element of a {List#}, or {void} if
      the list is empty.
      ###
      def head = fun(list)
        match list
        | Cons(head, _) -> head
        | _ -> void
        end
      end

      ###
      Returns everything BUT the first element of a {List#}, or
      {Nil()} if the list is already empty.
      ###
      def tail = fun(list)
        match list
        | Cons(_, tail) -> tail
        | _ -> List#empty
        end
      end

      ###
      Reverses the given {List#}, using cute recursion.

      For example, if we reverse {List#oneTwoThree}:
      @example#oneTwoThree
        reverse(List#oneTwoThree)
      @end
      We get {reverse#oneTwoThree}
      ###
      def reverse = fun(list)
        (fun reverse(list, reversed)
          match list
          | Nil() -> reversed
          | Cons(head, tail) ->
            reverse(tail, Cons(head, reversed))
          end
        end)(list, Nil())
      end
    `,
    repl: ['head(List#oneTwoThree)', 'tail(List#oneTwoThree)', 'reverse#'],
  },
];
