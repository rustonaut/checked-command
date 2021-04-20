use mapped_command::prelude::*;

fn ls_command() -> Command<Vec<String>, CommandExecutionWithStringOutputError> {
    Command::new(
        "ls",
        MapStdoutString(|out| {
            let lines = out.lines().map(Into::into).collect::<Vec<_>>();
            Ok(lines)
        }),
    )
}

fn main() {
    let entries = ls_command().run().unwrap();
    println!("ls:");
    for entry in entries {
        println!("\t{}", entry);
    }
}
