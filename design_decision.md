# Design Decisions

## Why this crate?

When using `Command` it is very easy to forget that the `Result`
returned by `output()` does not represent if the command was 
successful but just if it did run, possible unsuccessful. E.g.
a typlical error is to see something like this in a `build.rs`
script:

```
// bad, you have to check the value
// of the exit status, too
Command::new("cmd123")
    .arg("--with_spelling_errorr") 
    .status()
    .expect("cmd123 failed");
```

In which case `cmd123` would terminate with a non-successful exit
status but `status()` will still return `Ok(ExitStatus)`. Which
makes sense but especially in case of build scripts it is extremely
common that a non-successful exit status is also seen as failure.
This crate first makes it less probable to forget checking the
exit status and secondly removes part of the "check the exit status"
code.


## Why a custom Output

In std `Output` is a type bundling the collected output of stderr, stdout
and the exit status. But such a struct is not needed for this crate.
Firstly `Ok` is only returned if the exit status was successful i.e.
available and not equal 0. Which is a single value and therefore the `Ok`
site does not have to contain a exit status. On the `Err` side on the other
side there is no reason to close couple the output and exit status.
Secondly `ExitStatus` does not have a constructor (except on unix 
with the `ExitStatusExt`) which means testing a method taking 
`Output` as parameter can be bothersome.

## Why three errors?

This create has three very similar errors, which all have a `Io` variant
and a `Failure` variant only variating in the later like this:

- `StatusError::Failure(ExitStatus)` returned by `status()` and `wait()`
- `StatusErrorWithOutput::Failure(ExitStatus, Output)` returned by `output()` and `wait_with_output()`
- `Error::Failure(ExitStatus, Option<Output>)` created from the other two errors

Alternatives to this could be to only have the first two errors or always directly return
the third error. The reason why `status()` and `output()` return different errors is, that
`status()` never has a output while `output()` always has a output value in it's failure case
this means if you handle the error directly where it happens you would either have a option
you always ignore or a option you always unwrap, especially the later one is bad style and
is more brittle wrt. refactoring then having separate errors. Also in scenarios where you
directly handle the error, there is no drawback of having two errors.

On the other hand in scenarios where you propagate the error up e.g. with a `error_chain`
and don't care much about the output in failure case, except maybe loging it, having two
errors is a bit bothersome, therefore a third error which can represent both error cases
was added. Through the implicity conversion from `StatusError`/`StatusErrorWithOutput`
to `Error` when `try!{}`/`?` is used there is little congnitive overhead for this third
error.

Honestly the benefits and drawbacks (like more code) of this choice even out and
using it might is just personal preference. Allways using the third option would
be similar good.



## Why implement CheckedCommand/CheckedChild this way?

Both `CheckedCommand` and `CheckedChild` are structs with a single
field wrapping of type `Command`/`Child`. A naive approach would use
`Deref` to expose the inner structs methods, but this would defete the
purpose of this wrappers, which is to "hide away" the original `status`,
`output` implementations. And while shadowing the methods exposed by deref
with methods of the same name is possible it is also slightly brittle.
As `Command` and `Child` don't have neither to manny methods nor
are expected to gain (many) new ones I decided to create a explicit
wrapper, which "manually" proxies all unchanged methods. This also
has the big benefit that if `Command` or `Child` gains a new method,
which would have to be replaced by a checked version (like `Child::try_wait`)
it won't be automatically exposed. Because if it would automatically expose
the method it is easy to overlook that this method is not replaced with a checked
variation. With the current implementation the method would only by available by
explicitly getting a reference to the inner type, making clear that it isn't wrapped. 

