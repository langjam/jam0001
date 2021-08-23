#![cfg(test)]

use std::{cell::RefCell, collections::HashSet, io::Write, path::{Path, PathBuf}, rc::Rc};

#[test]
fn doc_gen() {
    check_program_run(
        "../programs/doc-gen/main.tnt".as_ref(),
        Some("../programs/doc-gen/output.md".as_ref()),
        None,
    );
}

#[test]
fn doc_test() {
    check_program_run(
        "../programs/doc-test/main.tnt".as_ref(),
        Some("../programs/doc-test/output.txt".as_ref()),
        None,
    );
}

#[test]
fn literate() {
    check_program_run(
        "../programs/literate/main.tnt".as_ref(),
        Some("../programs/literate/output.md".as_ref()),
        None,
    );
}

#[derive(Clone)]
struct SharedSink {
    result: Rc<RefCell<String>>,
}

impl Write for SharedSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let buf = std::str::from_utf8(buf).unwrap();
        (*self.result.borrow_mut()) += buf;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn do_run(source: &str) -> (String, String) {
    let ast = match tontuna::parse(source) {
        Ok(ast) => ast,
        Err(e) => return (
            "".to_owned(),
            format!("parse error at line {}: {}", e.span.start_line(), e.message),
        ),
    };
    let output = SharedSink {
        result: Default::default(),
    };
    let stderr = match tontuna::eval(&ast, Box::new(output.clone())) {
        Ok(()) => "".to_owned(),
        Err(e) => format!("runtime error at line {}: {}", e.span.start_line(), e.message),
    };
    let stdout: String = output.result.borrow().clone();
    (stdout, stderr)
}

fn check_program_run(
    path: &Path,
    stdout_path: Option<&Path>,
    stderr_path: Option<&Path>,
) {
    let source = std::fs::read_to_string(path)
        .expect(&format!("failed to read {:?}", path));
    let source = source.replace("\r\n", "\n");
    let stdout = match stdout_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace('\r', ""),
        None => "".to_owned(),
    };
    let stderr = match stderr_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace('\r', ""),
        None => "".to_owned(),
    };
    let (actual_out, actual_err) = do_run(&source);
    if actual_err != stderr {
        panic!(
            "program {} gave incorrect error, expected {:?}, got {:?}",
            path.display(),
            stderr,
            actual_err,
        );
    }
    if actual_out != stdout {
        panic!(
            "program {} gave incorrect output, expected {:?}, got {:?}",
            path.display(),
            stdout,
            actual_out,
        );
    }
}

fn file_exists(path: &Path) -> bool {
    match std::fs::metadata(path) {
        Ok(_) => true,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => false,
        Err(e) => panic!("io error: {}", e),
    }
}

fn add_extension(path: &Path, ext: &str) -> PathBuf {
    let mut path = path.to_path_buf().into_os_string();
    path.push(".");
    path.push(ext);
    PathBuf::from(path)
}

#[test]
fn test_cases() {
    let mut seen_files = HashSet::new();
    let mut used_files = HashSet::new();
    for entry in std::fs::read_dir("../programs/test-cases").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        seen_files.insert(path.clone());
        match path.extension() {
            None => continue,
            Some(x) if x != "tnt" => continue,
            Some(_) => {}
        }
        let stdout_path = add_extension(&path, "stdout");
        let stderr_path = add_extension(&path, "stderr");
        let stdout_path = file_exists(&stdout_path).then(|| stdout_path);
        let stderr_path = file_exists(&stderr_path).then(|| stderr_path);
        check_program_run(&path, stdout_path.as_deref(), stderr_path.as_deref());
        used_files.insert(path);
        used_files.extend(stdout_path);
        used_files.extend(stderr_path);
    }
    for file in &seen_files {
        if !used_files.contains(file) {
            panic!("file {} was not used", file.display());
        }
    }
}
