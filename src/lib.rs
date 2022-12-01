pub mod scanner;

pub fn run(source: &str) {
    println!("{:?}", scanner::run(source));
}