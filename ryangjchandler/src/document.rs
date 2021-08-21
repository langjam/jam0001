use colored::*;

#[derive(Debug)]
pub struct Document {
    name: String,
    description: Option<String>,
    author: Option<String>,
    version: Option<String>,
}

impl Document {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: None,
            author: None,
            version: None,
        }
    }

    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }

    pub fn set_description(&mut self, description: Option<String>) {
        self.description = description;
    }

    pub fn set_author(&mut self, author: Option<String>) {
        self.author = author;
    }

    pub fn set_version(&mut self, version: Option<String>) {
        self.version = version;
    }

    pub fn help(&self) {
        println!("{} {}", self.name.white(), if self.version.is_some() {
            self.version.as_ref().unwrap().green()
        } else {
            "".green()
        });

        if let Some(d) = self.description.clone() {
            println!("Description: {}", d.yellow())
        }

        if let Some(a) = self.author.clone() {
            println!("Author: {}", a.normal());
        }

        std::process::exit(0);
    }
}