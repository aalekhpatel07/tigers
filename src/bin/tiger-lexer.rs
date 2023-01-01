use std::path::PathBuf;

use tigers::lex::lex;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "Tiger Lexer")]
#[command(author, version, about, long_about = None)]
pub struct Args {
    pub files: Vec<PathBuf>,
}


pub fn main() {
    let args = Args::parse();
    for file in args.files {
        let s = std::fs::read_to_string(file.clone()).unwrap();

        match lex(&s) {
            Ok(tokens) => {
                println!("Found program in {}. Source:\n{}", file.to_string_lossy(), s);
                let len = tokens.len();
                println!("Tokens found {}:\n", len);
                for token in tokens {
                    println!("{:?}", token);
                }
            }
            Err(e) => {
                println!("Failed to lex {}:\n{}", file.to_string_lossy(), e);
            }
        }
    }
}