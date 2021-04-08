use mapped_command::{
    output_mapping::{CommandExecutionWithStringOutputError as Error, MapStdoutString},
    Command,
};

fn ls_command() -> Command<Vec<String>, Error> {
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
