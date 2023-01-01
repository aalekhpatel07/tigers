use std::{path::PathBuf, path::Path, error::Error};
use std::result::Result;
use tigers::{lex, Token};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "Tiger Lexer")]
#[command(author = "Aalekh Patel <aalekh.gwpeck.7998@icloud.com>", version, about, long_about = None)]
pub struct Args {
    pub files: Vec<PathBuf>,
}


pub fn process_file<P: AsRef<Path>>(path: P) -> Result<bool, Box<dyn Error + Sync + Send>> {
    let s = std::fs::read_to_string(path.as_ref())?;

    match lex(&s) {
        Ok(tokens) => {
            println!("Found program in {}. Source:\n{}", path.as_ref().to_string_lossy(), s);
            let len = tokens.len();
            println!("Tokens found {}:\n", len);
            for token in tokens {
                println!("{:?}", token);
            }
            Ok(true)
        }
        Err(e) => {
            println!("Failed to lex {}:\n{}", path.as_ref().to_string_lossy(), e);
            Ok(false)
        }
    }
}


pub fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = Args::parse();

    args.files.into_iter().for_each(|file| {
        let _ = process_file(file);
    });

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smol_file() {
        let args = Args {
            files: vec!["tests/smol.tig".into()],
        };
        assert!(process_file(args.files[0].clone()).unwrap());
    }
    #[test]
    fn test_big_file() {
        let args = Args {
            files: vec!["tests/large.tig".into()],
        };
        assert!(process_file(args.files[0].clone()).unwrap());
    }

    #[test]
    fn test_nonsense() {
        let args = Args {
            files: vec!["tests/broken.tig".into()],
        };
        assert!(!process_file(args.files[0].clone()).unwrap());
    }
}