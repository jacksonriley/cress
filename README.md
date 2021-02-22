## Cress
Simple (most likely) chess engine in Rust, to be written as a learning experience.

Not a great name - supposed to be a portmanteau of "chess" and "Rust"...

### Usage
Run the GUI with `cargo run`.
```
USAGE:
    cress [OPTIONS]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -m, --mode <mode>    What mode to run the program in [possible values: 2p, comp]
```
On Linux, [druid](https://github.com/linebender/druid#linux) requires gtk+3 - install [here](https://www.gtk.org/docs/installations/linux/).
If running on WSL, an X server is required, e.g. [Xming](https://sourceforge.net/projects/xming/). Tutorial [here](https://www.pcworld.com/article/3055403/windows-10s-bash-shell-can-run-graphical-linux-applications-with-this-trick.html).

Tests can be run with `cargo test`.