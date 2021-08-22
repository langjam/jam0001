use codespan_reporting as cr;
use structopt::StructOpt;
use std::path::PathBuf;

#[derive(StructOpt)]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,
    /// Output file, prints to stdout if not specified
    #[structopt(short, long, parse(from_os_str))]
    output: Option<PathBuf>,
    /// Only check for parse errors
    #[structopt(short, long)]
    check: bool,
}

fn main() {
    let opt = Opt::from_args();

    let path = opt.input;
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!("error: cannot read {}", path.display());
        eprintln!("    {}", e);
        std::process::exit(1);
    });

    let source = source.replace("\r\n", "\n");

    let ast = match tontuna::parse(&source) {
        Ok(ast) => ast,
        Err(e) => {
            print_diagnostic(&path.to_string_lossy(), &source, e);
            std::process::exit(1)
        }
    };

    if opt.check {
        return;
    }

    let output: Box<dyn std::io::Write> = match opt.output {
        Some(path) => {
            let file = match std::fs::File::create(&path) {
                Ok(file) => file,
                Err(e) => {
                    eprintln!("error: cannot create {}", path.display());
                    eprintln!("    {}", e);
                    std::process::exit(1);
                }
            };
            Box::new(file)
        }
        None => Box::new(std::io::stdout()),
    };

    match tontuna::eval(&ast, output) {
        Ok(()) => {}
        Err(mut e) => {
            e.message = format!("runtime error: {}", e.message);
            print_diagnostic(&path.to_string_lossy(), &source, e);
            std::process::exit(1);
        }
    }
}

fn print_diagnostic(
    file: &str,
    source: &str,
    error: tontuna::Error,
) {
    if print_diagnostics(file, &source, std::iter::once(error)).is_err() {
        std::process::exit(2);
    }
}

fn print_diagnostics(
    file: &str,
    source: &str,
    diagnostics: impl Iterator<Item = tontuna::Error>,
) -> anyhow::Result<()> {
    let stream = cr::term::termcolor::StandardStream::stderr(cr::term::termcolor::ColorChoice::Auto);
    let mut stream = stream.lock();

    let chars = codespan_reporting::term::Chars::ascii();

    let config = codespan_reporting::term::Config {
        chars,
        .. codespan_reporting::term::Config::default()
    };
    let mut files = cr::files::SimpleFiles::new();
    let file = files.add(file, source);
    for diagnostic in diagnostics {
        let severity = cr::diagnostic::Severity::Error;
        let diagnostic = cr::diagnostic::Diagnostic::new(severity)
            .with_message(diagnostic.message)
            .with_labels(vec![
                cr::diagnostic::Label::primary(file, diagnostic.span.source_range()),
            ]);

        cr::term::emit(&mut stream, &config, &files, &diagnostic)?;
    }
    Ok(())
}
