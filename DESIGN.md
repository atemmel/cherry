# Design

This is an ongoing design document dissecting the various pieces of the Cherry shell.

# Modules

Modules should be "easy enough" and direct in their usage. There should only be a select few ways to import a module. We can draw some parallels to the C convention of including headers:

```c
#include <stdio.h>        // include a system-wide header
#include "local_header.h" // include a relative header
```

Likewise, cherry provides marginally different syntax for importing a local/system-wide module:

```
import str     # import a system-wide module
import ./utils # import a relative module
```

# Syntax

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
