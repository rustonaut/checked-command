use std::{env, thread, time, io, process};
use std::io::Write;
use std::collections::HashMap;


// this will produce a binary which is used by
// integration tests to check if all the parts
// wrapped in CheckedCommand, etc. are there
// not that it is created through the `build.rs`
// script, so no cargo integration available!
// (but also not needed)
fn main() {
    // skip name of this bin and take first provided arg
    let args: Vec<String> = env::args().collect();
    assert!(args.len() >= 2);
    match args[1].as_str() {
        "hang" => do_hang(),
        "echo_line" => echo_line(),
        "check" => run_checks(&args[2..]),
        "noop" => {},
        "print" => { println!("test123") },
        "output_with_err" => output_with_err(),
        "noop_err" => { process::exit(113); }
        e => panic!("unexpected subcommand: {}", e)
    }
}

fn output_with_err() {
    println!("test123");
    process::exit(100);
}

fn do_hang() {
    for _ in 0..10 { thread::sleep(time::Duration::from_secs(1));  }
    panic!("hang is expected to be killed, why wasn't it?");
}

fn echo_line() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    write_out(io::stdout(), &*line);
    //emmit twice on err to make a difference
    write_out(io::stderr(), &*line);
    write_out(io::stderr(), &*line);
}

fn write_out<W: Write>(mut v: W, line: &str) {
    let len = write!(&mut v, "{}", line).unwrap();
    v.flush().unwrap();
}


macro_rules! map {
    ($($key:expr => $val:expr),*) => {{
        let mut map = HashMap::new();
        $(map.insert($key.into(), $val.into());)*
        map
    }}
}
fn run_checks(args: &[String]) {
    //check current_dir/arg/args
    assert_eq!(args, &[
        env::current_dir().unwrap().to_str().unwrap().to_string(),
        "one".into(), "two".into(), "three".into()]);

    let envs: HashMap<String, String> = env::vars().collect();
    // for env_clean, env_remove, env and envs
    let expected: HashMap<String, String> = map! {
        "key1" => "one",
        "key2" => "two",
        "key3" => "three"
    };

    assert_eq!(expected, envs);


}