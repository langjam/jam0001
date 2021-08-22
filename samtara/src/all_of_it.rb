require 'set'
require 'pp'

DEBUG = false

def debug(*args)
  if DEBUG
    puts *args
  end
end

class String
  def matches(re)
    start_at = 0
    matches  = [ ]
    while(m = match(re, start_at))
      matches.push(m)
      start_at = m.end(0)
    end
    matches
  end
end

def join_pattern(pattern)
  pattern.map do |part|
    case part.first
    when :var
      "<#{part.last}>"
    when :plain
      part.last
    end
  end.join
end

class MatchData
  def match_length
    self.end(0) - self.begin(0)
  end
end

def run(code)
  input, rules = code.split(/\n-{3,}/m)
  rules = parse(rules || '')
  last_input = nil
  debug input
  debug
  while input != last_input do
    last_input = input.clone
    found = rules.map do |rule|
      shortest_match = input.matches(rule[:header_regex]).min do |a, b|
        a.match_length <=> b.match_length
      end
      [rule, shortest_match]
    end.reject { |(_, match)| match.nil? }
      .min_by do |(rule, match)|
        [match.match_length, match.named_captures.keys.length]
      end

    if found
      found_rule, found_match = found
      rewrite = run_rule(found_match, found_rule[:rewrite])
      if DEBUG
        debug "via #{join_pattern(found_rule[:header])} #{found_match.named_captures.inspect} (length #{found_match.match_length})"
        before_input = input.clone
        after_input = input.clone
        current = before_input[found_match.begin(0)...found_match.end(0)]
        before_input[found_match.begin(0)...found_match.end(0)] = "\e[31m#{current}\e[0m"
        puts before_input
        puts
        after_input[found_match.begin(0)...found_match.end(0)] = "\e[32m#{rewrite}\e[0m"
        puts "#{after_input}"
        puts
      end
      input[found_match.begin(0)...found_match.end(0)] = rewrite
    end
    gets
  end
  input
end

def run_rule(variables, match_pattern)
  unless variables.is_a?(MatchData)
    raise ArgumentError, "Need MatchData, got #{variables.class}"
  end
  unless match_pattern.is_a?(Array)
    raise ArgumentError, "Need Array, got #{match_pattern.class}"
  end
  match_pattern.map do |item|
    case item.first
    when :plain
      item.last
    when :var
      variables[item.last] || "👻"
    end
  end.join
end

def to_regex(pattern, all: false)
  # [[:plain, ""], [:var, "X"]]
  past_captures = Set.new
  regex_string = pattern.map do |item|
    case item.first
    when :plain
      "#{Regexp.quote(item.last)}"
    when :var
      capture_name = item.last
      if past_captures.include?(capture_name)
        "\\k<#{capture_name}>"
      else
        past_captures << capture_name
        if item.last.match?(/[A-Z]/)
          "(?<#{capture_name}>.+?)"
        else
          "(?<#{capture_name}>.)"
        end
      end
    end
  end.join
  if all
    Regexp.new("^#{regex_string}$")
  else
    Regexp.new(regex_string)
  end
end

# :: String -> [Rule]
def parse(rules)
  rules.split("\n").map do |line|
    if line.start_with?("  ")
      [:rewrite, parse_pattern(line.chars.drop(2).join)]
    elsif line.start_with?(/\S/)
      [:header, line]
    elsif line.empty?
      next
    else
      raise "Invalid line: #{line.inspect}"
    end
  end.compact.reduce([]) do |rules, token|
    case token.first
    when :header
      pattern = parse_pattern(token.last)
      rules << {
        header: pattern,
        header_regex: to_regex(pattern),
        rewrite: nil
      }
    when :rewrite
      rules.last[:rewrite] = token.last
    else
      raise "Invalid token: #{token.inspect}"
    end
    rules
  end
end

# "foo <X> bar <Y> baz"
# [[:plain, "foo "], [:var, "X"], [:plain, " bar "], [:var, "Y"], [:plain, " baz"]]
def parse_pattern(pattern)
  before, var, after = pattern.partition(/<.>/)
  if var.empty?
    [[:plain, before]]
  else
    [[:plain, before], [:var, var[1]]] + parse_pattern(after)
  end
end

def try(code)
  result = run(code)
  puts "\n----------------"
  puts result
  puts "----------------"
  puts result.inspect
  exit 0
end

TESTS = [
  {
    name: 'Sanity check, no rules',
    code: "hello\n---",
    result: "hello",
  },
  {
    name: 'Paren balancing',
    code: <<~BO,
      not(yes))
      ----
      not(yes)
        no
      not(no)
        yes
    BO
    result: "no)"
  },
  {
    name: 'Lots of not',
    code: <<~BO,
      not(not(not(not(yes))))
      ----
      not(yes)
        no
      not(no)
        yes
    BO
    result: "yes"
  },
  {
    name: 'Double up',
    code: <<~BO,
    double(boom)
    ---
    double(<X>)
      <X><X>
    BO
    result: 'boomboom'
  },
  {
    name: 'Repeating strings',
    code: <<~BO,
    repeat hi [|||] times;
    ----
    repeat <X> [<Y>] times;
      repeat() <X> [<Y>] times;

    repeat() <X> [<Y>|] times;
      repeat(<X>) <X> [<Y>] times;

    repeat(<R>) <X> [|] times;
      repeat(<R><X>) <X> [] times;

    repeat(<R>) <X> [<Y>|] times;
      repeat(<R><X>) <X> [<Y>] times;

    repeat(<R>) <X> [] times;
      <R>
    BO
    result: 'hihihi'
  },
  {
    name: 'boolean logic',
    code: <<~BO,
    👍 and 👎
    ----
    👍 and 👍
      👍

    <X> and <Y>
      👎

    BO
    result: '👎'
  },
  {
    name: 'Addition',
    code: <<~BO,
    [|||] + [||]
    ---
    [<X>] + [<Y>]
      [<X><Y>]
    BO
    result: '[|||||]'
  },
  {
    name: 'Subtraction',
    code: <<~BO,
    [|||] - [||]
    ---
    [<X>|] - [<Y>|]
      [<X>] - [<Y>]

    [<X>|] - [<Y>|]
      [<X>] - [<Y>]

    [<X>|] - [|]
      [<X>] - []

    [] - [<Y>]
      []

    [<X>] - []
      [<X>]
    BO
    result: '[|]'
  },
  {
    name: 'Multiplication',
    code: <<~BO,
    [|||] * [||||]
    ---
    [<X>] * [<Y>]
      *([]) [<X>] [<Y>]

    *([]) [<X>] [<Y>|]
      *([<X>]) [<X>] [<Y>]

    *([<R>]) [<X>] [<Y>|]
      *([<R><X>]) [<X>] [<Y>]

    *([<R>]) [<X>] [|]
      *([<R><X>]) [<X>] []

    *([<R>]) [<X>] []
      [<R>]

    BO
    result: '[||||||||||||]'
  },
  {
    name: 'Split in two (Unification)',
    code: <<~BO,
    split ||||||;
    ---------
    split <X><X>;
      <X> <X>
    BO
    result: "||| |||"
  },
  {
    name: 'No-remainder division',
    code: <<~BO,
    [||||||||||||] / [|||]
    ----------------------
    [<X>] / [<Y>]
      /() [<X>] [<Y>]

    /(<R>) [<Y>] [<Y>]
      [<R>|]

    /() [<X><Y>] [<Y>]
      /(|) [<X>] [<Y>]

    /(<R>) [<X><Y>] [<Y>]
      /(<R>|) [<X>] [<Y>]

    /(<R>) [<X>] [<Y>]
      [<R>]
    BO
    result: '[||||]'
  },
  {
    name: 'Chained addition',
    code: <<~BO,
    [|] + [||] + [|||]
    ----------------------
    [<X>] + [<Y>]
      [<X><Y>]
    BO
    result: '[||||||]'
  },
  {
    name: 'Chained subtraction',
    code: <<~BO,
    [||||] - [||] - [|]
    ----------------------
    [<X><Y>] - [<Y>]
      [<X>]
    BO
    result: '[|]'
  },
  {
    name: 'Modulo',
    code: <<~BO,
    [|||||] % [||]
    ----------
    [<X><Y>] % [<Y>]
      [<X>] % [<Y>]

    [<X>] % [<Y>]
      [<X>]
    BO
    result: '[|]'
  },
  {
    name: 'Parallel statements',
    code: <<~BO,
    first hello;
    first world;
    -----------
    first <x><X>;
      <x>
    BO
    result: "h\nw"
  },
  {
    name: 'All in a row',
    code: <<~BO,
    print each letter of hello world.
    -----------
    print each letter of <x><X>.
      <x> print each letter of <X>.

    print each letter of <x>.
      <x>
    BO
    result: "h e l l o   w o r l d"
  },
  {
    name: 'Fallback operator',
    code: <<~BO,
    ?:bar
    ----
    ?:<Y>
      <Y>

    ?<X>:<Y>
      <X>
    BO
    result: "bar"
  },
  {
    name: 'Fizzbuzz, Part one: fizz',
    code: <<~BO,
    fizz-check fizz [||||||] [|||].
    ----
    fizz-check <M> [<R><D>] [<D>].
      fizz-check <M> [<R>] [<D>].

    fizz-check <M> [<D>] [<D>].
      <M>

    fizz-check <M> [<R>] [<D>].
      fail
    BO
    result: "fizz"
  },
  {
    name: 'Fizzbuzz, Part two: also buzz',
    code: <<~BO,
    fizz-num [|];
    fizz-num [||];
    fizz-num [|||];
    fizz-num [||||];
    fizz-num [|||||];
    fizz-num [||||||];
    fizz-num [|||||||];
    fizz-num [||||||||];
    fizz-num [|||||||||];
    fizz-num [||||||||||];
    fizz-num [|||||||||||];
    fizz-num [||||||||||||];
    fizz-num [|||||||||||||];
    fizz-num [||||||||||||||];
    fizz-num [|||||||||||||||];

    ----

    fizz-num [<N>];
      ?fizz-check "fizz" [<N>] [|||].fizz-check "buzz" [<N>] [|||||].:[<N>]!

    fizz-check "<M>" [<R><D>] [<D>].
      fizz-check "<M>" [<R>] [<D>].


    fizz-check "<M>" [<D>] [<D>].
      <M>

    fizz-check "<M>" [<R>] [<R><D>].
      

    ?<A>:<B>!
      <A>

    ?:<B>!
      <B>
    BO
    result: <<~RES
    [|]
    [||]
    fizz
    [||||]
    buzz
    fizz
    [|||||||]
    [||||||||]
    fizz
    buzz
    [|||||||||||]
    fizz
    [|||||||||||||]
    [||||||||||||||]
    fizzbuzz
    RES
  },
  {
    name: 'Decimal to unary',
    code: <<~BO,
    dec(3) dec(21)
    --------------

    dec(<N>)
      [decb(<N>, |)]

    decb(<n>, <M>)
      rep(<n>, <M>)

    decb(<R><n>, <M>)
      decb(<R>, <M><M><M><M><M><M><M><M><M><M>)rep(<n>, <M>)

    rep(0, <B>)
      
    rep(1, <B>)
      <B>
    rep(2, <B>)
      <B><B>
    rep(3, <B>)
      <B><B><B>
    rep(4, <B>)
      <B><B><B><B>
    rep(5, <B>)
      <B><B><B><B><B>
    rep(6, <B>)
      <B><B><B><B><B><B>
    rep(7, <B>)
      <B><B><B><B><B><B><B>
    rep(8, <B>)
      <B><B><B><B><B><B><B><B>
    rep(9, <B>)
      <B><B><B><B><B><B><B><B><B>

    BO
    result: '[|||] [|||||||||||||||||||||]'
  },

  #Current pattern	      111	110	101	100	011	010	001	000
  #New state for center cell	0	1	1	0	1	1	1	0
  {
    name: 'Rule 110',
    focus: true,
    code: <<~BO,
    ^|0000000000000010000000000000000000$
    ---

    0|00
      00|0
    0|01
      01|1
    0|10
      01|0
    0|11
      01|1
    1|00
      10|0
    1|01
      11|1
    1|10
      11|0
    1|11
      10|1
    BO
    result: '?'
  }
]
def test
  ok = 0
  skip = 0

  if (focus = TESTS.find{ |t| t[:focus] })
    run_test(focus)
    return
  else
    TESTS.each do |test|
      if test[:skip]
        skip += 1
        next
      end
      next if test[:skip]
      ok += run_test(test) ? 1 : 0
    end
  end

  puts
  puts "Done. #{ok}/#{TESTS.count - skip} tests passed, #{skip} skipped."
end

def run_test(test)
  begin
    puts "===== #{test[:name]}"
    if (actual = run(test[:code])) != test[:result]
      puts "Code:"
      puts test[:code]
      puts
      puts "Expected:"
      puts '```'
      puts test[:result]
      puts '```'
      puts
      puts "Actual:"
      puts '```'
      puts actual
      puts '```'
      puts '=========================='
      puts test[:result].inspect
      puts "but got"
      puts actual.inspect
      false
    else
      true
    end
  rescue Exception => e
    puts e
    puts e.backtrace.join("\n")
    puts
    false
  end
end

test

