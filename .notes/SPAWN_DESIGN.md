# **This document is OUTDATED but keeping it is useful especial wrt. the use cases.**

# Spawn Design IDEAS (not the current design)

This document was used to collect ideas about how spawn can be implemented,
but since then wasn't updated and as such doesn't represent the current
spawn design. It's still interesting especially wrt. the use cases.
If this project would be bigger I likely would move this into another
repository to prevent confusion.

## Execution Steps

In following is a list of steps which are
done when running a sub-process.

The steps are **unordered** some might be
skipped for some use-cases (e.g. writing
input) and others might be merged for
some use-cases (e.g. reading output and
awaiting termination).

1. spawning the process
2. yielding control to the outside
3. gain back control
4. reading outputs
5. writing inputs
6. awaiting termination
7. mapping outputs


## Use Cases

### Case0: Only Read Output

In this case we just wan to read the input
library version 0.3.0 covers this just fine,
no spawning is needed.

This will do in order steps 1 then
4+6 then 7.

### Case1: Just Delay Waiting

This is similar to Case0 but between
spawning the process and awaiting it
some other work is intended to be done.

This will do in order steps 1 then
2 then 3 then 4+6 then 7.

### Case2: Single Fixed Input

Like Case1 but in between spawning and
awaiting the result a single fixed input
is send the the subprocess.

In it's simplest and default form this
does *not* allow any interaction between
the read output and the written input at
all.

"Single fixed input" here means it's a
single "I write input" step. This means
it can mean writing e.g. a password over
stdin but it could also mean writing a
list of entries you on-the-fly retrieve
from some channel or similar.

For this you do run in order steps 1(spawn) then
2(yield contrl) then 5(input) then 3(gain contrl)
then 4+6(output+term) then 7(mapping).

### Case3: Disjoint Parallel Read/Write

In this case you concurrently (and parallel)
write and read from/to the sub-process. This
case does not allow any interaction between read
and write.

As we don't consider async I/O for now this
mean you run Case2 with an background output
capture thread (see below).

I.e. you run step 1 (spawn) then 4.1(spawn output)
then 2(yield contrl) then 5(input) then 3(gain contrl)
then 4.2+6(output+term) then 7.

This could for example be used for a streaming file
converter or similar.

### Case4: Interlocking Dialog

In the simples version this switches in a interlocking
manner between reading input and writing outputs. (A
more complex version of this might read input and write
outputs potentially in parallel and then syncs and then
repeats).

In difference to previous steps this will repeatably
map outputs instead of just doing this once. It is
also much more entangled with the calling code and
might need a explicit termination condition.

A simple case of this would be to read input util
you get a "username: " prompt then write the username
to the input then read outputs until you get a "password: "
prompt then write the password then continue to read
output and add them to some final "result".

This likely interacts with some state machine.

We don't need to support this in any form in the
next spawn design, but it would be nice.

### Case6.*: Repeated Mapping

The cases Case6.0 - Case6.3 are variations of Case0-3 where
chunks of output are repeatedly read and mapped.

This differs as mapping is not done once but multiple times.

In case of Case2 this might have a single fixed input per
repeated mapping or a single fixed input per process
execution.

Case6.0-6.2 might be possible to emulate with Case4, this
might not work with Case3 unless Case3 is tweaked in ways
which might not be preferable.

## Background Output Capture

Some of the cases can be run in a way that the output is captured
in the background through a spawned thread. This is useful as the
buffer of the stdout pipe is limited. So if a lot of time passes
between spawning and awaiting input the buffer might fill up
"hanging" the sub-process.

This is a option independent of the cases if used then step

"4. reading outputs"

becomes

"4.1 spawning a output capture thread"

"4.2. retrieving/awaiting captured output"

It doesn't make sense to run this with Case0, but
it should be useable with Case1 & 2. For Case3 it
or something similar is mandatory. In Case4 it
might become a bit complex but should be possible
anyway.

## Async I/O?

The input/output writing could under some circumstances
be turned in to async I/O. Through this likely adds
a lot of complexity some patterns might require GAL
for them to work. So I will not purse this for now.

## Exec Callback & Spawning

The current design uses a nicely mockable "exec callback" where you pass
in the settings and it runs the program and returns the result.

This allow easily mocking the result in ways which can include not
at all running the command which is very nice for testing.

This also means all the types needed around/during the execution
are encapsulated in the "exec callback"

Normal execution is done by a preset callback which setups the
`std`lib's command and then runs `wait_for_output`.

This doesn't scale to spawn based interfaces.


### Generator based approach (hypothetical)

Theoretical for at least Cases0-3 we could use a generator.

Through this is purely hypothetical as this would need both
generator support and generic associated types (GAT).

If this would be supported we could:

1. start running the generator
2. depending on settings we spawn a background output capture thread
3. it will spawn the process and yield the input stream if required
4. do anything necessary, including potentially writing to the input
5. continue running the generator will wait on output or wait on the
   background output capture thread.
5. map the output and yield the result.

For Case4 it will yield a intermediate result instead and repeat.

### Trait based approach

A practical way to reach to goal is to have trait/trait object
instead of a function/function trait object.

Ignoring mocking and case4+ we could do something like

1. call some "spawn" method which will spawn the thread and return stdio
2. potentially spawn a background output capture thread using the returned stdio.out/stdio.err
3. return a "child" handle which grants access to stdio.in
4. once the "wait" method is called either it waits on the output or it
   waits on the background capture thread which waits on the output. This
   return the raw output
4. then output mapping is done (not in the trait!)

Through the "spawn" method must only be called once and always before the "wait"
method which also must only be called onc.

Luckily `self: Box<Self>` is trait object safe, so we likely can implement this nicely.

```rust
trait ExecImpl {
    fn spawn(self: Box<Self>, opts: {
        background capture thread,
        ...?
    }) -> Result<Box<dyn InnerChild>, ???>;
}

trait InnerChild {
    fn take_stdin(&mut self) -> Option<ChildStdin>; //Option<Box<dyn ProcessInput>>
    // only Some if stdout is "Piped" but output mapping does not map the output
    fn take_stdout(&mut self) -> ..;
    // same limits as stdout
    fn take_stderr(&mut self) -> ..;


    fn wait(self: Box<Self>) -> Result<CapturedOutputs, ???>
}
```

Now the question is how *this conflicts with mocking*:

1. It is more verbose, but we can fix that later.
2. We might have no real child so we can't return ChildStdin because
   we need to be able to return nothing bound to std `sys` internals.

But `ChildStdin` only implements the various from/into `RawFd` traits,
`Debug`, `Into<Stdio>` and `Write`.

So what we can easily do is a `trait ProcessInput: Debug + Write`.

Problem include the conversion from/to `RawFd` as there might not
be a `RawFd` and `Into<Stdio>` which needed to e.g. connect the
input of one sub-process to the output of another.

So I would guess something like following could work:

```rust
// this is a bit tricky as a `dyn Write` isn't necessary the most preferment  think to do.
trait ProcessInput: Debug + Write {
    /// If it can be converted to an RawFd it will be converted.
    /// Normally this should only work if this wraps a `ChildStdin`.
    fn into_raw_fd(self: Box<Self>) -> Result<RawFd, Box<Self>>;
}
```

Now that we have this we can implement it for mock functions,
either by directly implementing them on the function or by
implementing it on a `Mock(pub <func>)` wrapper.

Simple Mock functions would work by running to completion on
`ExecImpl` and then just return the already existing
input on `InnerChild` as well as erroring on writes to
`Input` (if it was configured to be contained).


With this the child returned from command spawn will be
something like:

```rust
struct Child<??MAPPING??> {
    inner: Box<dyn InnerChild>
}
```


But with `ProcessInput`/`ProcessOutput` traits we could not
have a `InnerChild` trait but instead do something like:

```rust
// we use Arc<dyn SpawnImpl>
trait SpawnImpl {
    fn spawn(&self,  options: SpawnImplOptions) -> ChildParts;
}

trait ChildWaiter {
    fn wait(self: Box<Self>) -> Result<(), io::Error>;
}

trait ProcessInput: Debug + io::Write + 'static/*+ IntoRawFd,IntoRawHandle*/ {
    // in many situations the dynamic dispatch of Write isn't a problem,
    // especially *not* if e.g. write_all is used. But in some other
    // cases it can be a perf-degredation. In this case you can consider
    // generic code and then either call it with the trait object as
    // fallback if downcasting to ChildStdin failed.
    fn as_write_mut(&mut self) -> &mut dyn Write + 'static;
    fn as_write_ref(&self) -> &dyn Write + 'static;
}

trait ProcessOutput: Debug + io::Read + 'static /*+ IntoRawFd/IntoRawHandle*/ {
    fn as_read_mut(&mut self) -> &mut dyn Read + 'static;
    fn as_read_ref(&mut self) -> &dyn Read + 'static;
}

struct ChildParts {
    stdout: Option<Box<dyn ProcessOutput>>,
    stderr: Option<Box<dyn ProcessOutput>>,
    stdin: Option<Box<dyn ProcessInput>>,
    waiter: Box<dyn ChildWaiter>
}
```