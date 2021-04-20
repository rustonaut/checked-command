
# mapped-command

*Version 0.2.x is a very thin wrapper to `std::process::Command` and can be found here: [in the 0.2 branch](https://github.com/rustonaut/checked-command/tree/0.2)*

Provides an alternative to rust's `std::process::Command` which is more testable, flexible and prevents the way to easy class of bugs where the programmer forgets to check the exit
status of a process as intuition tels us a "failed command" should return a error. (But the
error in `std::process::Command` is about failing to launch a sub-process and doesn't care
about exit codes at all).

For now this is focused on cases which wait until the subprocess is completed
and then map the output (or do not care about the output).

Currently this type contains following features:

- by default check the exit status

- bundle a mapping of the captured stdout/stderr to an result into the command,
  i.e. the `Command` type is `Command<Output, Error>` e.g. `Command<Vec<String>, Error>`.

- implicitly define if stdout/stderr needs to be captured to prevent mistakes
  wrt. this, this is done through through the same mechanism which is used to
  define how the output is mapped, e.g. `Command::new("ls", ReturnStdoutString)`
  will implicitly enabled stdout capturing and disable `stderr` capturing.

- allow replacing command execution with an callback meant for mocking

- besides allowing to decide weather the sub-process should inherit the environment and
  which variables get removed/set/overwritten this type also allows you to whitelist which
  env variables should be inherited.

- do not have `&mut self` pass through based API. This makes it more bothersome to create
  functions which create and return commands, which this types intents to make simple so
  that you can e.g. have a function like `fn ls_command() -> Command<Vec<String>, Error>`
  which returns a command which if run runs the ls command and returns a vector of string
  (or an error if spawning, running or utf8 validation fails).

- be generic over Output and Error type but dynamic over how the captured stdout/err is
  mapped to the given `Result<Output, Error>`. This allows you to e.g. at runtime switch
  between different function which create a command with the same output but on different
  ways (i.e. with different called programs and output mapping, e.g. based on a config
  setting).

# Mini Example

Use `cargo run --example readme` to run this:

```rust
use mapped_command::prelude::*;

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
```

For other examples e.g. about how the mocking works take a look at the [examples dir](./examples/) or the module level documentation produced by rustdoc which likely should be hosted [on docs.rs](https://docs.rs/mapped-command). Be aware that the link leads to the latest released version and might as such be out of sync if updates have not yet been released.

## Contribution

See the license terms below, issues and PR's are welcome.

### About running tests

Many tests only run if the `mocking` feature is enabled, as such run tests with the different features enabled/disabled.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.