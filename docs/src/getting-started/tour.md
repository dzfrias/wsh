# A Tour of wsh

On a base level, `wsh` aims to be similar to
[bash](https://www.gnu.org/software/bash/) and
[zsh](https://zsh.sourceforge.io/). Every-day usage will resemble the
aforementioned languages quite a bit. However, `wsh` differs a lot in its
[programmatic features](../using/programming/index.md). `wsh` can resemble that
of a modern scripting language, if needed. You'll find that the control flow,
arithmetic, syntax, and semantics are much less arcane than the POSIX standard.

## The base usage

To get started, start up `wsh` and run the `cd` [built-in](../using/builtins.md)
to move to your home directory.

```
$ cd
$ echo hi from wsh!
hi from wsh!
$ pwd
/Users/dzfrias
```

Piping, from POSIX-style shells, remains the same:

```
$ echo "how many words here?" | wc -w
       4
```

Redirection is also present:

```
$ echo "put this in a file" > out.txt
$ echo "append this" >> out.txt
$ cat out.txt
put this in a file
append this
```

Capture the stdout of a command with backticks:

```
$ cat `echo Cargo.toml`
...
```

Create an [alias](../using/configuation.md) with the `alias` key word:

```
$ alias foo = echo I don't want to type this again
$ foo
I don't want to type this again
```

Hopefully, these tools are enough to get you started using `wsh`! See the
[POSIX migration](../posix-migration.md) chapter for some more differences
between `wsh` and traditional shells. You can read a more detailed dive into
running commands in [this chapter](../using/running-commands.md).

## wsh as a scripting language

While a lot of the every-day usage experience will be similar to that of
traditional shells, `wsh` has a powerful escape hatch. You can use the `.`
character to escape to a language akin to a regular scripting language:

```
$ .x = 10
$ echo .(x + 100 == 110)
true
$ echo .("string" + " concat")
string concat
$ # yes, there's type coersion
$ .(1 + "1")
2
```

For the full power of language, create a new file, `example.wsh`.

```
echo hello from this script!
.var = 10
while var > 0 do
  echo .var
  .var = var - 1
end

if var == 0 then
  echo BLAST OFF!
else
  echo basic math broke in the shell...
end
```

Run the script with the `source` [built-in](../using/builtins.md).

```
$ source example.wsh
hello from this script!
10
9
8
7
6
5
4
3
2
1
BLAST OFF!
```

You can read more about `wsh`'s programmatic capabilities in
[this chapter](../using/programming/index.md).
