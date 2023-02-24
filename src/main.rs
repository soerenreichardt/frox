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

fn run_file<'a>(path: &str, mut frox: FroxRunner<'a>) {
    let source = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    match frox.run(source.into()) {
        Ok(_) => (),
        Err(error) => println!("{}", error),
    }
}

fn run_prompt<'a>(mut frox: FroxRunner<'a>) {
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        stdin.lock().read_line(&mut buffer).unwrap();
        buffer = buffer.trim().to_string();
        if buffer.is_empty() {
            return;
        }
        match frox.run(buffer.into()) {
            Ok(_) => (),
            Err(error) => println!("{}", error),
        }
    }
}
