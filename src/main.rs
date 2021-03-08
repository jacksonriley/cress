mod engine;
mod gui;
use clap::{App, Arg};
use std::str::FromStr;

fn main() {
    let matches = App::new("Cress")
        .version("0.1")
        .author("Jackson Riley")
        .about("Basic chess engine with GUI")
        .arg(
            Arg::with_name("mode")
                .help("What mode to run the program in")
                .short("m")
                .long("mode")
                .possible_values(&["2p", "comp"])
                .takes_value(true),
        )
        .arg(
            Arg::with_name("depth")
                .help("Number of half moves to look ahead")
                .short("d")
                .long("depth")
                .takes_value(true),
        )
        .get_matches();
    let mode = gui::Mode::from_str(matches.value_of("mode").unwrap_or("comp")).unwrap();
    let depth: u8 = matches.value_of("depth").unwrap_or("4").parse().unwrap();
    gui::main(mode, depth);
}
