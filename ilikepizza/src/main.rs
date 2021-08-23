use std::{fmt, io::Write};
use structopt::StructOpt;

/// Incredibly inefficient execution engine for this painful programming
/// language
struct Vm<I: Io = StandardIo> {
    pub(crate) increment: isize,
    pub(crate) position: isize,
    pub(crate) lines: Vec<String>,
    pub(crate) stack: Vec<isize>,
    pub(crate) io: I,
    user_is_annoying: bool,
}

#[derive(Debug)]
enum ExecError {
    UnknownOp(String),
    CantParseValue,
    MissingValue,
    MissingComment,
    OutOfBounds,
}

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            // Piss off the programmer
            Self::UnknownOp(ref s) => writeln!(f, "You doofus! {:?}? That's not a valid instruction!", s),
            Self::CantParseValue => writeln!(f, "What's that? Don't you know you can only write integers after an instruction?"),
            Self::MissingValue => writeln!(f, "That instruction was expecting at least one more value on the stack! Now it's sad!"),
            Self::MissingComment => writeln!(f, "Yes, that instruction needs a comment before it to work. Yes, it's insane. No, I don't care."),
            Self::OutOfBounds => writeln!(f, "Stack OOB. This kind of error seems to be a constant across all programming languages huh?"),
        }
    }
}

// Makes debugging the print/input instructions much easier (see test module
// below)
trait Io: Default {
    fn input(&mut self) -> Option<isize>;
    // `String` instead of `&str` was a dumb decision...
    fn print(&mut self, string: String);
    fn print_ascii(&mut self, ascii: isize) {
        let mut string = String::with_capacity(1);
        if 0 <= ascii && ascii < 258 {
            string.push(ascii as u8 as char);
        } else {
            string.push('?');
        }
        self.print(string);
    }
}

/// Use stdin and stdout for I/O
#[derive(Default)]
struct StandardIo;

impl Io for StandardIo {
    fn input(&mut self) -> Option<isize> {
        use std::{io, io::BufRead};

        io::stdout().lock().flush().unwrap();
        let stdin = io::stdin();
        let buf = &mut String::new();
        stdin.lock().read_line(buf).ok()?;
        buf.trim().parse().ok()
    }

    fn print(&mut self, string: String) {
        print!("{}", string);
    }
}

impl<I: Io> Vm<I> {
    /// Create an empty VM
    pub(crate) fn new() -> Self {
        Self {
            increment: 1,
            position: 0,
            lines: Vec::new(),
            stack: Vec::new(),
            io: I::default(),
            user_is_annoying: false,
        }
    }

    /// Create a VM and append lines to execute
    pub(crate) fn with_lines(lines: &str) -> Self {
        let mut vm = Self::new();
        vm.append(lines);
        vm
    }

    pub(crate) fn append(&mut self, lines: &str) {
        let mut lines: Vec<String> = lines
            .trim()
            .lines()
            .map(|line| line.trim().to_owned())
            .collect();

        self.lines.append(&mut lines);
    }

    /// Is finished? (true if out of bounds)
    pub(crate) fn is_finished(&self) -> bool {
        self.position < 0 || self.position as usize >= self.lines.len()
    }

    fn extract_values(line: &str) -> Result<Vec<isize>, ExecError> {
        line.split_whitespace()
            .skip(1)
            .filter_map(|value_as_str| {
                if value_as_str.is_empty() {
                    return None;
                }

                let value: isize = match value_as_str.parse() {
                    Ok(n) => n,
                    Err(_) => return Some(Err(ExecError::CantParseValue)),
                };

                Some(Ok(value))
            })
            .collect()
    }

    /// Toggle comment for a line, does nothing if the position doesn't exist
    fn toggle(&mut self, at: isize) {
        if at < 0 || at as usize >= self.lines.len() {
            return;
        }

        let line_ref: &mut String = &mut self.lines[at as usize];

        if line_ref.starts_with('#') {
            // Uncomment
            line_ref.remove(0);
        } else {
            // Comment
            line_ref.insert(0, '#');
        }
    }

    /// Check if a position index exists
    fn is_valid(&self, index: isize) -> bool {
        0 <= index && (index as usize) < self.lines.len()
    }

    /// Edit all valid lines in given range, ignoring invalid range values
    fn edit_range(
        &mut self,
        a: isize,
        b: isize,
        mut f: impl FnMut(&mut String),
    ) {
        let a = self.position + (a * self.increment);
        let b = self.position + (b * self.increment);

        let (start, end) = if a < b { (a, b) } else { (b, a) };

        for i in start..=end {
            if self.is_valid(i) {
                f(&mut self.lines[i as usize]);
            }
        }
    }

    /// Print a string using the VM's printer
    fn print(&mut self, string: String) { self.io.print(string); }

    /// Pop a value from the stack and convert it to a boolean
    fn pop_bool(&mut self) -> Result<bool, ExecError> { Ok(self.pop()? != 0) }

    /// Pop a value from the stack or error out if none available
    fn pop(&mut self) -> Result<isize, ExecError> {
        match self.stack.pop() {
            Some(value) => Ok(value),
            None => Err(ExecError::MissingValue),
        }
    }

    /// Remove value at index from the stack
    fn try_remove(&mut self, index: isize) -> Result<isize, ExecError> {
        if 0 <= index && (index as usize) < self.stack.len() {
            Ok(self.stack.remove(index as usize))
        } else {
            Err(ExecError::OutOfBounds)
        }
    }

    /// Push a value onto the stack
    fn push(&mut self, n: isize) { self.stack.push(n); }

    /// Push a boolean onto the stack
    fn push_bool(&mut self, b: bool) { self.stack.push(b as isize); }

    /// Get the previous comment as a string
    fn previous_comment(&self) -> Result<String, ExecError> {
        let position = self.position - self.increment;
        if !self.is_valid(position) {
            return Err(ExecError::MissingComment);
        }
        let mut string = self.lines[position as usize].clone();
        if !string.starts_with('#') {
            return Err(ExecError::MissingComment);
        }
        string.remove(0);
        Ok(string)
    }

    /// Use previous comment as a template to format the number with
    fn format(&self, n: isize) -> Result<String, ExecError> {
        let line = self.previous_comment()?.replace('%', &n.to_string());
        Ok(line)
    }

    /// Wait until the user inputs a single integer
    fn input(&mut self) -> isize {
        const MESSAGES: [&'static str; 4] = [
            "Please, just pick an integer: ",
            "Pretty please?: ",
            "Yawn...: ",
            "I'm stubborn but even I would've chosen an integer by now!: ",
        ];

        if self.user_is_annoying {
            self.print("I'm picking 42, just letting you know.\n".into());
            return 42;
        }

        let mut level_of_annoyance = 0;
        loop {
            if let Some(value) = self.io.input() {
                break value;
            }

            if let Some(message) = MESSAGES.get(level_of_annoyance) {
                level_of_annoyance += 1;
                self.print(message.to_string());
            } else {
                self.user_is_annoying = true;
                self.print(concat!(
                        "AS PER MY LAST MESSAGES, you should've picked a value by now. ",
                        "I've decided we agreed on 42.\nYou're welcome.\n",
                ).into());
            }
        }
    }

    /// Execute the current line
    fn exec_line(&mut self, mut line: &str) -> Result<(), ExecError> {
        line = line.trim();

        if line.is_empty() || line.starts_with('#') {
            // Just a comment, nothing scary (yet...)
            return Ok(());
        }

        let instruction = match line.split_whitespace().next() {
            Some(x) => x,
            None => return Ok(()), // empty line => no op
        };

        // Syntax sugar: my-instruction 5 8 => push 5 8; my-instruction
        let values_to_push = &mut Self::extract_values(line)?;
        self.stack.append(values_to_push);

        match instruction {
            "push" => {
                // Push values onto the stack (already done!)
            },
            "reverse" => {
                // Reverse execution order and toggle next instruction
                self.increment = -self.increment;
                self.toggle(self.position + self.increment);
            },
            "comment-range" => {
                // Comment out a range of lines
                let start = self.pop()?;
                let end = self.pop()?;
                self.edit_range(start, end, |line| line.insert(0, '#'));
            },
            "uncomment-range" => {
                // Uncomment a range of lines
                let start = self.pop()?;
                let end = self.pop()?;
                self.edit_range(start, end, |line| {
                    if line.starts_with('#') {
                        line.remove(0);
                    }
                });
            },
            "toggle-range" => {
                // Uncomment comments and comment other lines in a range of
                // lines
                let start = self.pop()?;
                let end = self.pop()?;
                self.edit_range(start, end, |line| {
                    if line.starts_with('#') {
                        line.remove(0);
                    } else {
                        line.insert(0, '#');
                    }
                });
            },
            "add" => {
                // Add the 2 values on top of the stack together
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(a + b);
            },
            "subtract" => {
                // Subtract the first value on the stack from the second
                let first = self.pop()?;
                let second = self.pop()?;
                self.push(second - first);
            },
            "multiply" => {
                // Multiply the 2 values on top of the stack together
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(a * b);
            },
            "divide" => {
                // Divide the second value on the stack with the first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push(second / first);
            },
            "modulo" => {
                // Second value on the stack modulo the first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push(second % first);
            },
            "equal" => {
                // Test if the 2 value on top of the stack are equal
                let a = self.pop()?;
                let b = self.pop()?;
                self.push_bool(a == b);
            },
            "not-equal" => {
                // Test if the 2 value on top of the stack are different
                let a = self.pop()?;
                let b = self.pop()?;
                self.push_bool(a != b);
            },
            "inferior" => {
                // Test if the second value of the stack is inferior to the
                // first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push_bool(second < first);
            },
            "inferior-or-equal" => {
                // Test if the second value of the stack is inferior or equal to
                // the first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push_bool(second <= first);
            },
            "superior" => {
                // Test if the second value of the stack is superior to the
                // first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push_bool(second > first);
            },
            "superior-or-equal" => {
                // Test if the second value of the stack is superior or equal to
                // the first
                let first = self.pop()?;
                let second = self.pop()?;
                self.push_bool(second >= first);
            },
            "and" => {
                // Test if the first 2 values on the stack are true
                let first = self.pop_bool()?;
                let second = self.pop_bool()?;
                self.push_bool(second && first);
            },
            "or" => {
                // Test if either the first 2 values on the stack are true
                let first = self.pop_bool()?;
                let second = self.pop_bool()?;
                self.push_bool(second || first);
            },
            "not" => {
                // Test if first value on the stack is false
                let value = self.pop_bool()?;
                self.push_bool(!value);
            },
            "comment-if" => {
                // Comment next instruction if value on the stack is true
                if self.pop_bool()? {
                    self.edit_range(1, 1, |line| line.insert(0, '#'));
                }
            },
            "uncomment-if" => {
                // Uncomment next instruction if value on the stack is true
                if self.pop_bool()? {
                    self.edit_range(1, 1, |line| {
                        if line.starts_with('#') {
                            line.remove(0);
                        }
                    });
                }
            },
            "toggle-if" => {
                // Toggle next instruction if value on the stack is true
                if self.pop_bool()? {
                    self.edit_range(1, 1, |line| {
                        if line.starts_with('#') {
                            line.remove(0);
                        } else {
                            line.insert(0, '#');
                        }
                    });
                }
            },
            "duplicate" => {
                // Duplicate the value on top of the stack
                let n = self.pop()?;
                self.push(n);
                self.push(n);
            },
            "duplicate-at" => {
                // Duplicate a value at the given index from the top of the
                // stack (1 indexed)
                let position = self.pop()?;
                let value = if 0 < position
                    && (position as usize) <= self.stack.len()
                {
                    self.stack[self.stack.len() - (position as usize)]
                } else {
                    return Err(ExecError::OutOfBounds);
                };
                self.push(value);
            },
            "pop" => {
                // Remove the topmost value of the stack
                self.pop()?;
            },
            "pop-at" => {
                // Remove value at specified index from the top of the stack (1
                // indexed)
                let index = self.pop()?;
                self.try_remove(self.stack.len() as isize - index)?;
            },
            "print-format" => {
                // Print previous comment, replacing '%' with the value on the
                // stack
                let n = self.pop()?;
                let line = self.format(n)? + "\n";
                self.print(line);
            },
            "print-ascii" => {
                // Print a single ascii character with the value taken from the
                // stack Print '?' if not valid ascii and can't
                // be utf8
                let ascii = self.pop()?;
                self.io.print_ascii(ascii);
            },
            "input" => {
                // Input a single integer using previous comment as a prompt
                let prompt = self.previous_comment()? + ": ";
                self.print(prompt);
                let value = self.input();
                self.push(value);
            },
            _ => {
                // Unknown instruction => annoying error
                return Err(ExecError::UnknownOp(instruction.into()));
            },
        }

        Ok(())
    }

    /// Execute the whole program
    pub(crate) fn exec(&mut self) -> Result<(), ExecError> {
        while !self.is_finished() {
            self.exec_next()?;
        }

        Ok(())
    }

    /// Execute the current line and increment the execution's position
    pub(crate) fn exec_next(&mut self) -> Result<(), ExecError> {
        let line = if self.is_finished() {
            return Ok(());
        } else {
            self.lines[self.position as usize].clone()
        };

        self.exec_line(&line)?;

        // Go to the next instruction
        self.position += self.increment;

        Ok(())
    }

    /// Print information about the current execution state
    fn dump(&mut self) {
        // Super duper inefficient

        let mut string = self
            .lines
            .iter()
            .enumerate()
            .map(|(index, line)| {
                let maybe_arrow = if self.position == index as isize {
                    "here ->"
                } else {
                    "       "
                };

                format!("{} | {}\n", maybe_arrow, line)
            })
            .collect::<Vec<_>>()
            .join("");

        if self.is_finished() {
            string
                .push_str("\nhere -> ~somewhere else~ (execution finished)\n");
        }

        string.push_str(&format!("\nStack     = {:?}\n", self.stack));
        string.push_str(&format!("Increment = {:?}\n", self.increment));

        string.push_str("\n===\n\n");
        string.insert_str(0, "\n\n===\n\n");

        self.print(string);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use std::{fs, io, io::prelude::*};

    let opt = cli::Opt::from_args();
    let program = &fs::read_to_string(&opt.file)?;
    let vm = &mut Vm::with_lines(program);

    fn clear_screen() {
        print!("\x1B[2J\x1B[1;1H");
        io::stdout().lock().flush().unwrap();
    }

    fn handle_result(vm: &mut Vm, result: Result<(), ExecError>) {
        use std::process;

        let err = match result {
            Ok(()) => return,
            Err(x) => x,
        };

        clear_screen();
        vm.dump();
        eprintln!("ERROR: {}", err);

        process::exit(1)
    }

    if opt.step_by_step {
        while !vm.is_finished() {
            clear_screen();
            vm.dump();

            print!("Press ENTER to continue...");
            io::stdout().lock().flush().unwrap();
            io::stdin().lock().lines().next().unwrap().unwrap();

            let result = vm.exec_next();
            handle_result(vm, result);
        }

        clear_screen();
        vm.dump();
    } else {
        let result = vm.exec();
        handle_result(vm, result);
    }

    Ok(())
}

mod cli {
    use std::path::PathBuf;
    use structopt::StructOpt;

    #[derive(StructOpt)]
    #[structopt(rename_all = "kebab-case")]
    pub(crate) struct Opt {
        /// Perform execution step by step
        #[structopt(short, long)]
        pub(crate) step_by_step: bool,

        /// File to execute
        #[structopt(parse(from_os_str))]
        pub(crate) file: PathBuf,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// It keeps the stuff to print in a string instead of actually printing it!
    /// Also picks input values from an input stack rather than stdin.
    /// Makes testing much easier!
    #[derive(Default)]
    struct TestIo {
        printed: String,
        inputs: Vec<isize>,
    }

    impl Io for TestIo {
        fn input(&mut self) -> Option<isize> { self.inputs.pop() }

        fn print(&mut self, string: String) { self.printed.push_str(&string); }
    }

    /// Assert stack equality after executing the program
    fn test_stack(program: &str, expected_stack: &[isize]) {
        let mut vm = Vm::<TestIo>::with_lines(program);
        vm.exec().unwrap();
        assert_eq!(vm.stack.as_slice(), expected_stack);
    }

    /// Assert printed string equality after executing the program
    fn test_printed(program: &str, expected_printed: &str) {
        let mut vm = Vm::<TestIo>::with_lines(program);
        vm.exec().unwrap();
        assert_eq!(vm.io.printed.as_str(), expected_printed);
    }

    #[test]
    fn test_append_lines() {
        let vm = Vm::<TestIo>::with_lines("hello \n world\n");
        assert_eq!(vm.lines.as_slice(), &["hello", "world"]);
    }

    #[test]
    fn test_push() { test_stack("push 5 55  8 ", &[5, 55, 8]) }

    #[test]
    fn test_reverse() { test_stack("#push 1 \n reverse", &[1]) }

    #[test]
    fn test_exec() { test_stack("push 8 \n push 5", &[8, 5]) }

    #[test]
    fn test_comment_range() {
        test_stack("comment-range 1 2 \n 1 \n 2 \n push 1 ", &[1])
    }

    #[test]
    fn test_uncomment_range() {
        test_stack("uncomment-range 1 2 \n#push 1\npush 1\n#add 1", &[1, 1])
    }

    #[test]
    fn test_add() { test_stack("add 1 2", &[3]) }

    #[test]
    fn test_equal() {
        test_stack("equal 5 5", &[1]);
        test_stack("equal 5 4", &[0]);
    }

    #[test]
    fn test_not_equal() { test_stack("not-equal 5 4", &[1]) }

    #[test]
    fn test_duplicate() { test_stack("duplicate 2", &[2, 2]) }

    #[test]
    fn test_subtract() { test_stack("subtract 10 5", &[5]) }

    #[test]
    fn test_toggle_range() {
        test_stack("toggle-range 1 2 \n #push 1 \n push 2", &[1])
    }

    #[test]
    fn test_duplicate_at() { test_stack("duplicate-at 0 1 2", &[0, 1, 0]) }

    #[test]
    fn test_pop() { test_stack("pop 1", &[]) }

    #[test]
    fn test_pop_at() { test_stack("pop-at 0 1 2", &[1]) }

    #[test]
    fn test_input() {
        let mut vm = Vm::<TestIo>::with_lines("#A \n input");
        vm.io.inputs.push(4);
        vm.exec().unwrap();
        assert_eq!(vm.io.printed, "A: ");
        assert_eq!(vm.stack.as_slice(), &[4]);
    }

    #[test]
    fn test_print_format() {
        test_printed(
            "#The value % is % \n print-format 5",
            "The value 5 is 5\n",
        )
    }

    #[test]
    fn test_print_ascii() {
        test_printed("print-ascii 5", "\x05");
        test_printed("print-ascii -1", "?");
    }
}
