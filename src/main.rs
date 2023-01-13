use std::env;
use std::fs;
use std::io::{self, BufRead, Write};

use frox::FroxRunner;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let frox = FroxRunner::new();
    match args.as_slice() {
        [_, path] => run_file(path, frox), 
        [_] => run_prompt(frox),
        _ => panic!("Unknown arguments")
    }
}

fn run_file(path: &str, mut frox: FroxRunner) {
    let source = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    match frox.run(&source) {
        Ok(_) => (),
        Err(error) => println!("{}", error),
    }
}

fn run_prompt(mut frox: FroxRunner) {
    let mut buffer = String::new();
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        buffer.clear();
        stdin.lock().read_line(&mut buffer).unwrap();
        buffer = buffer.trim().to_string();
        if buffer.is_empty() {
            return;
        }
        match frox.run(&buffer) {
            Ok(_) => (),
            Err(error) => println!("{}", error),
        }
    }
}
