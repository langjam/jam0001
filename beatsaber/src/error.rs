use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;
pub type Label = codespan_reporting::diagnostic::Label<()>;

#[derive(Clone)]
pub struct Reporter<'a> {
    source: SimpleFile<&'a str, &'a str>,
}

impl<'a> Reporter<'a> {
    pub fn new(source: &'a str, file: &'a str) -> Self {
        Self {
            source: SimpleFile::new(file, source),
        }
    }

    pub fn report(&self, diagnostic: &Diagnostic) {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        term::emit(&mut writer, &config, &self.source, diagnostic).ok();
    }

    pub fn report_and_exit(&self, diagnostic: &Diagnostic) -> ! {
        self.report(diagnostic);
        std::process::exit(1)
    }
}
