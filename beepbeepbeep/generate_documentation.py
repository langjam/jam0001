import re

grammar_path = "src/grammar.lalrpop"
documentation_path = "DOCUMENTATION.md"

# sorry for the crimes i'm committing
phrase_pattern = re.compile(r"([a-zA-Z]+): \(\) = \{\n((?:    [^=]+ => \(\),\n)+)\}")
phrase_inner_pattern = re.compile(r"    ([^=]+) => \(\),\n")
operation_pattern = re.compile(r"([a-zA-Z]+): Op = \{\n((?:    [^=]+ => Op::[a-zA-Z]+(\([^\)]*\))?,\n)+)\}")
operation_inner_target_pattern = re.compile(r"    [^=]+ => Op::([a-zA-Z]+(\([^\)]*\))?),\n")
operation_inner_pattern = re.compile(r"    ([^=]+) => Op::[a-zA-Z]+(\([^\)]*\))?,\n")

with open(grammar_path) as f:
    grammar = f.read()

phrases: dict[str, list[str]] = {}
for phrase in re.finditer(phrase_pattern, grammar):
    name, inner = phrase.groups()
    for part in re.finditer(phrase_inner_pattern, inner):
        phrases.setdefault(name, []).append(part.group(1))

operations: dict[str, list[str]] = {}
for operation in re.finditer(operation_pattern, grammar):
    name = operation.group(1)
    inner = operation.group(2)
    target_match = re.match(operation_inner_target_pattern, inner)
    target = target_match.group(1) if target_match is not None else "appeasing the type checker"
    for part in re.finditer(operation_inner_pattern, inner):
        operations.setdefault(name, []).append(re.sub("<[a-zA-Z]+:", "<", part.group(1)))

sorted_dict = lambda d: sorted(list(d.items()), key=lambda x: x[0])

output = [
    "# Operation Documentation",
    "This file contains every valid way to spell out an operation. "
    "Strings enclosed in quotes should be interpreted literally, separated by any amount of whitespace. "
    "Capitalized and unquoted words refer to *common phrase patterns*, enumerated below.",
    "\nDo note that this page is auto-generated. "
    "If it's incorrect anywhere, feel free to file an issue / PR.",
    "\n## Common phrases",
    "This is an exhaustive list of common phrases that are used in *operation* patterns, or recursively in other common phrase patterns."
]

for phrase, variants in sorted_dict(phrases):
    output.append(f"\n### `{phrase}`")
    for variant in variants:
        output.append(f"* `{variant}`")

output.append("\n## Operations")
output.append("This is an exhaustive list of operations. Values surrounded in angle brackets refer to special patterns captured in the output.")

for operation, variants in sorted_dict(operations):
    output.append(f"\n### `{operation}`")
    for variant in variants:
        output.append(f"* `{variant}`")

output.append("\n## Special patterns")
output.append("These patterns don't fit into other categories.")

output.append("\n### `<Name>`")
output.append("* A sequence of 1 or more ASCII alphabetic characters.")

output.append("\n### `<Names>`")
output.append("* A comma-delimited list of 0 or more `<Name>` patterns.")

output.append("\n### `<Nth>`")
output.append("* An ordinal number, e.g. `1st`, `15th`, `103rd`.")

output.append("\n### `<Num>`")
output.append("* A sequence of 1 or more ASCII digit characters.")

output.append("\n### `<Quote>`")
output.append("* A string inside double quotes. Escapes are parsed identically to Rust, allowing most common escapes as well as `\\uXXXX` / `\\U{XXXXXX}`.")

with open(documentation_path, "w") as f:
    f.write("\n".join(output))

