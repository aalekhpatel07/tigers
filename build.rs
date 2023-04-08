use std::{path, str::FromStr};

use lalrpop;

fn main() {
    lalrpop::process_root().unwrap();
}
