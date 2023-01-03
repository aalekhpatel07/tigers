use std::path::PathBuf;

use rustyline::error::ReadlineError;
use rustyline::{Editor, Result, Helper, KeyEvent, EventHandler, Cmd, KeyCode, Modifiers};
// use rustyline::validate::{MatchingBracketValidator, Validator};
use rustyline_derive::{
    Completer,
    Helper,
    Highlighter,
    Hinter,
    Validator,
};
use clap::{Parser, Subcommand};

use tigers::lex;

// #[derive(Completer, Helper, Highlighter, Hinter, Validator)]
// pub struct InputValidator {
//     #[rustyline(Validator)]
//     brackets: MatchingBracketValidator,
// }


#[derive(Parser, Debug, Clone)]
#[command(name = "tiger-repl")]
#[command(
    author = "Aalekh Patel <aalekh.gwpeck.7998@icloud.com>",
    version,
    about = "A REPL for debugging the Tiger compiler.",
    long_about = None
)]
pub struct Args {
    #[command(subcommand)]
    pub command: Action,
    #[arg(
        short, 
        long, 
        value_name = "FILE",
        value_hint = clap::ValueHint::FilePath
    )]
    pub history_file: Option<PathBuf>
}


#[derive(Subcommand, Debug, Clone)]
pub enum Action {
    Lex,
    Parse,
}

pub const DEFAULT_HISTORY_FILE: &str = "./history.txt";

pub fn handle_input(input: &str, action: &Action) {
    match action {
        Action::Lex => {
            match lex(input) {
                Ok(tokens) => {
                    let len = tokens.len();
                    println!("Tokens found: {}", len);
                    let mut string = String::new();
                    for token in tokens {
                        string.push_str(&format!("{} ", token));
                    }
                    println!("{}", string);
                    
                }
                Err(e) => {
                    println!("Failed to lex {}:\n{}", input, e);
                }
            }
        }
        Action::Parse => {
            println!("Parsing {}", input);
        }
    }

}

fn main() -> Result<()> {

    let mut args = Args::parse();
    if args.history_file.is_none() {
        args.history_file = Some(DEFAULT_HISTORY_FILE.into());
    }

    let history_file = args.history_file.clone().unwrap();
    let mut rl = Editor::<()>::new()?;
    rl.bind_sequence(
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline)
    );

    if rl.load_history(&history_file.clone()).is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                handle_input(line.as_str(), &args.command);
            },
            Err(ReadlineError::Interrupted) => {
                // CTRL-C so just skip
            },
            Err(ReadlineError::Eof) => {
                // CTRL-D so exit
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history(&history_file.clone()).expect("Couldn't save history.");
    Ok(())
}