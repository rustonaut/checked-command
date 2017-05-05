
# checked-command &emsp; [![Build Status](https://travis-ci.org/dathinab/checked-command.svg?branch=master)](https://travis-ci.org/dathinab/checked-command)

**extension to `std::process::Command` which adds a output/status considering the programs `ExitStatus`**

---

This create contains extension traits for `Command`/`Child` adding variations of
`status()`,`output()`, `wait()`, `wait_with_output()` which also check the exit status
when deciding wether to return `Ok(...)` or `Err(...)`. It also provides a
`CheckedCommand` and `CheckedChild` wrapper which replaces it's `status()`, etc.
methods with the checked variations.


Documentation can be [viewed on docs.rs](docs.rs/checked_command).
 
Motivations for why it's designed like it is can be found in the [design_decision.md file](./design_decision.md).


## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
