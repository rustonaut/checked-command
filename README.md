
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

# Missing Features

## output_without_exit_status

instead of directly using `std::process::Output` a custion `Output`
struct will be used, which strips the ExitStatus from the Output

TODO implement
TODO consider using a use_process_output feature instead

## CheckedCommand/CheckedChild (wrapper)

wrap `std::process::Command` forwarding all functions _except_ `status`/`output` etc.
So that you cannot accidentally use the unchecked version. Nice for build scripts and
`cargo script` usages

# TODO before v1.0

1. decide wether to keep 3 errors or to only have the error with the optional output
2. decide if wether or not to focus/provide a `CheckedCommand`/`CheckedChild` wrapper
3. decide if `status_code.success()` means we don't care about the status code
    - e.g. in Unix code != 0 <--> success, so don't care, but what's with other platforms
    - if so remove the `use_std_output` feature, it's now (mostly) useless
    - possible change the return type of `status`/`wait` to `Result<(), _>`
   

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
