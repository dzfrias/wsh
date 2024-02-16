# Configuration

Configuration in `wsh` is largely done through the `.wshrc` file. This file will
be executed on startup! `.wshrc` has different locations depending on your
operating system, for example:

| Linux                  | macOS                   | Windows                   |
| ---------------------- | ----------------------- | ------------------------- |
| `/home/dzfrias/.wshrc` | `/Users/dzfrias/.wshrc` | `C:\Users\dzfrias\.wshrc` |

Make sure to replaced `dzfrias` with your machine's name. If the file does not
already exist, feel free to just create it at that location! There are three
main things to put in a config file:

1. [Aliases](#aliases)
2. [Functions](#functions)
3. [Environment variables](#environment-variables)

## Aliases

Aliases allow you to set up custom names for [commands](./running-commands.md)
and their arguments. For example, here are two aliases for `echo`:

```
alias foo = echo
alias bar = echo hello world
```

Now, when in the shell,

```
$ foo nice
nice
$ bar
hello world
```

Make sure to restart the shell when a change to the `.wshrc` file is made!
Aliases can also represent pipelines:

```
alias long = echo hello world | wc -w | xargs word count:
```

```
$ long
word count: 2
```

## Functions

**[Main chapter](./programming/functions.md)**

Functions are _much_ more powerful than aliases, and allow you to create custom
commands with arbitrarily complex behavior!

```
def func : x do
  echo hello world
  if x > 3 then
    echo greater than 3!
  else
    echo less than or equal to 3!
  end
end
```

This function uses [conditionals](./programming/conditions.md), and takes in an
argument.

```
$ func 10
hello world
greater than 3!
$ func 1
hello world
less than or equal to 3!
```

For more details on the nuances of functions in `wsh`, see the
[Functions](./programming/functions.md) chapter.

> Note: Functions **must** be declared at the top level of your program. This,
> for example, is not allowed by the parser:
>
> ```
> if true then
>   def func do
>     echo hi
>   end
> end
> ```
>
> Aliases, however, **can** be declared conditionally.

## Environment variables

**[Main chapter](./environment.md)**

You can set environment variables using the `export` keyword. This will allow
all spawned subprocesses read the variable.

```
export HELLO = this is the value
```

In the shell,

```
$ echo $HELLO
```
