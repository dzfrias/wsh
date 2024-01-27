# Running Commands

Running commands in `wsh` is similar to that of other shells, following the
`<COMMAND> <ARGS>...` standard.

```
$ echo "Hello, World!"
Hello, World!
```

## Piping

Piping can be achieved with the `|` (pipe) token:

```
$ echo pipe! | tee out.txt | xargs echo cool
cool pipe!
$ cat out.txt
pipe!
```

This will pipe stdout, **not** stderr. The stderr stream can easily be
redirected into stdout, if needed:

```
$ cargo build %| wc -l
       1
```

> Note: `%|` in `wsh` is equivalent to `2>&1` in POSIX shells; see
> [the migration guide](../posix-migration.md) for similar discrepencies.

## Redirection

You can redirect to different files with the `>` and `>>` characters. `>` will
create (or overwrite) a file and fill it with the received input. `>>` is the
same as `>`, but will **append** to the file instead of overwriting it if it
already exists.

```
$ echo hello > out.txt
$ echo append >> out.txt
$ cat out.txt
hello
append
```

They can accept any expression on the right hand side (i.e.
[string concatenation](./programming/strings.md)):

```
$ echo important stuff > .("hi" + ".txt")
```

Like the regular [pipe](#piping) operator, you can use the `%` character to
redirect stderr along with stdout:

```
$ cargo build %> cargo_out.txt
$ cargo build %>> cargo_out.txt
```

> Note: Unlike POSIX shells, redirect symbols **must** terminate a pipeline. For
> example, the parser will not succeed with:
>
> ```
> echo hello > out | cat
> ```

## Output capture

You can use backticks to capture a command result into a
[string](./programming/strings.md):

```
$ .x = `cat large_file.txt`
$ .x
...
```

The above example uses variable [assignment](./programming/assignment.md).

## Status codes

You can get the exit status of any command using the `?` character. This will be
stored as a [number](./programming/numbers.md).

```
$ msityped
wsh: command not found: msityped
$ echo .? .(? + 100)
127 227
```
