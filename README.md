
# checked-command &emsp; [![Build Status](https://travis-ci.org/dathinab/checked-command.svg?branch=master)](https://travis-ci.org/dathinab/checked-command)

**extension to `std::process::Command` which adds a output/status considering the programs `ExitStatus`**

---

The normal `std::process::Command::output()` does not consider the 
`ExitStatus` of the run command, only returning `Err(...)` if there was
some kind of io-error. This crate provides extension a extension trait
adding a `checked_output()` method which only returns `Ok(Output)` if
`ExistStatus::success()` is true. The returned error will, depending on
the error case, still continue the captured output. Additionaly a 
`checked_status()` is provided and both methods are also implemented
on `std::process::Child`.


## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
