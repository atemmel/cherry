# Reference

This is an ongoing language reference document dissecting the various pieces of the Cherry shell.

# Syntax

## Comments

Single-line comments start with a hash sign (`#`).

```
# This is a comment.
```

Multi-line comments start with a `#{` and are terminated by a `#}`.

```
#{
This
is
a
multi-line
comment.
#}
```

Unlike most languages, (C, C++, Java, Go, C#, etc.) Multi-line comments can be nested. This is useful if you, during development, wish to comment out a block of code that already includes a multi-line comment.

```
#{
This is a comment.
#{
This is a nested comment.
#}
#}
```

(Inspirations: Shell, Matlab)

## Literals

Cherry works with a number of way to define literals to be used as values.

### Strings

`Strings` represents a sequence of arbitrary characters. A string can be written as:

```
"hello" # this is a string literal
```

You can also create multi-line string literals by enclosing its contents in `\``:

```
`
Lorem ipsum
dolor sit amet
`
```

(Inspirations: Most languages, Go)

### Barewords

`Barewords` are a literal most developers might recognize from other shells. A bareword is roughly explained as some arbitrary string of characters prefixed and terminated by a space.

If you look at the following line of Bash:
```bash
ls -l /tmp
```
The strings `-l` and `/tmp` are considered barewords.

An alternative definition of a bareword could be "a string so simple in terms of contents it can be left unquoted".

(Inspirations: Shell)

(Inspirations: Shell, Bash)

### Integers

If a bareword solely consists of numbers, it is interpreted to be an integer literal.

```
123 # this is an integer literal
```

(Inspirations: Most languages)

### Lists

A list is a collection of values who are accessed using an index. An index is any positive integer, starting from 0.  Lists can be constructed in different ways:

```
[]                       # this is an empty list of 0 elements
[ James Mary Kate Alex ] # this is a list of 4 elements, containing first names
[ 1 2 3 5 ]              # this is a list of 5 elements, beginning the fibonacci sequence
```

(Inspirations: JavaScript, Python, Shell)

### Records

A record is a key-value mapping in which values of arbitrary complexity can are accessed using a string (the key).
```
[=] # this is an empty record
```

## Running processes/function calls

Starting an external process and calling a function share their syntax.

```
ls      # runs the external process 'ls'
my-func # calls the function named 'my-func'
```

Eventual arguments are provided after the invocation, separated by spaces.

```
ls -l /tmp # runs the external process 'ls' with the arguments '-l' and '/tmp'

say Hello guys # calls the function named 'say' with the arguments 'Hello' and 'guys'
```


## Modules

`import` is used to import a module.

```
import str
```

Due to being a shell, the string describing the module is able to be represented as a bareword.

You can also alias an import.

```
import str s
```

All of the members of the `str` module are now instead accessed using `s`.

# Modules

Modules should be "easy enough" and direct in their usage. There should only be a select few ways to import a module. We can draw some parallels to the C convention of including headers:

```c
#include <stdio.h>        // include a system-wide header
#include "local_header.h" // include a relative header
```

Likewise, Cherry provides marginally different syntax for importing a local/system-wide module:

```
import str     # import a system-wide module
import ./utils # import a relative module
```

(Inspirations: Go, C)
