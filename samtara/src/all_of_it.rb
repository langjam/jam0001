require 'pp'

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

def show_compose(rules)
  rules.each do |rule|
    puts to_regex(rule[:header]).inspect
    puts "  match #{rule[:match].inspect}"
    rule[:clauses].each do |clause|
      puts "    #{to_regex(clause.first).inspect} -> #{clause.last.inspect}"
    end
    puts
  end
end

def run(code)
  input, rules = code.split(/\n-{3,}/m)
  rules = parse(rules || '')
  show_compose(rules)
  pp rules
  last_input = nil

  puts "-> #{input}"
  while input != last_input do
    last_input = input.clone
    rules.find do |rule|
      rule_regex = to_regex(rule[:header])
      shortest_match = input.matches(rule_regex).min do |a, b|
        a.length <=> b.length
      end
      if shortest_match
        puts "Using rule #{rule_regex.inspect}, match #{shortest_match.inspect}"
        rewrite = run_rule(match_target(shortest_match, rule[:match]), rule[:clauses])
        input[shortest_match.begin(0)...shortest_match.end(0)] = rewrite
        puts "===> #{input}"
        true
      else
        false
      end
    end
  end
  input
end

def match_target(variables, match_pattern)
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

def run_rule(target, clauses)
  clauses.each do |clause| 
    clause_regex = to_regex(clause.first, all: true)
    if clause_regex.match?(target)
      clause_match = clause_regex.match(target)
      return match_target(clause_match, clause.last)
    end
  end
  "❓"
end

def to_regex(pattern, all: false)
  # [[:plain, ""], [:var, "X"]]
  regex_string = pattern.map do |item|
    case item.first
    when :plain
      "#{Regexp.quote(item.last)}"
    when :var
      if item.last.match?(/[A-Z]/)
        "(?<#{item.last}>\\S*)"
      else
        "(?<#{item.last}>.)"
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
    if line.start_with?("  match ")
      [:match, line.chars.drop(8).join]
    elsif line.start_with?("    ")
      [:clause, line.chars.drop(4).join]
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
      rules << {
        header: parse_pattern(token.last),
        match: nil,
        clauses: []
      }
    when :match
      rules.last[:match] = parse_pattern(token.last)
    when :clause
      clause = token.last
      pattern, result = clause.split(/\s+->\s+/)
      rules.last[:clauses] << [
        parse_pattern(pattern),
        parse_pattern(result)
      ]
    else
      raise "Invalid token: #{token.inspect}"
    end
    rules
  end
end

def parse_pattern(pattern)
  pattern.partition(/<.>/).reject(&:empty?).map do |bit|
    if bit.match?(/<.>/)
      [:var, bit[1]]
    else
      [:plain, bit]
    end
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
        match yes
          <X> -> no
      not(no)
        match no
          <X> -> yes
    BO
    result: "no)"
  },
  {
    name: 'Lots of not',
    code: <<~BO,
      not(not(not(not(yes))))
      ----
      not(yes)
        match yes
          <X> -> no
      not(no)
        match no
          <X> -> yes
    BO
    result: "yes"
  },
  {
    name: 'Double up',
    code: <<~BO,
    double(boom)
    ---
    double(<X>)
      match <X>
        <X> -> <X><X>
    BO
    result: 'boomboom'
  }
]
def test
  ok = 0

  if (focus = TESTS.find{ |t| t[:focus] })
    run_test(focus)
    return
  else
    TESTS.each do |test|
      ok += run_test(test) ? 1 : 0
    end
  end

  puts
  puts "Done. #{ok}/#{TESTS.count} tests passed"
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
      puts test[:result].inspect
      puts '```'
      puts
      puts "Actual:"
      puts '```'
      puts actual.inspect
      puts '```'
      puts '=========================='
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

