# syntax

## operators
```
# ? - truthiness operator

? "hello" - true
? "" - false
? 1 - true        # unsure
? 0 - false       # unsure
? [ x ] - true
? [] - false
? true - true
? false - false
```

## modules

```
import the-module
import a-folder/the-module
import the-website.com/the-module

the-module::the-fn

$the-module::the-var

# export things in modules with `pub`
```

## functions/lambdas
```
fn my-proc {

}

fn my-proc my-arg-1 my-arg-2 {

}

my-argfree-lambda := { }

my-lambda-with-args := fn my-arg-1 { }

```

## error handling
```
stdout, exit-code := (ls /my/dir)

if ? $exit-code {
    # handle error
}
```

## tables/records
```
table := [
    name = james
    age  = 55
]

$table[name] = bond

$table::name  = bond
```

## lists
```
table := [ james jimmy jones ]

$table[0] = bond

$table::0  = bond    # unsure
```

## typing syntax

note: I'm not sure types will do great in this context, at least not yet

```

# variable declarations should ideally *not* require types

x := "my str"
y := 0

# function declarations should (perhaps always) require types

fn add-person person-to-add: Person list-of-persons: []Person -> []Person {
    ...
}

# type declarations can mainly be done using records or enums

# enum
type Relation = "family" | "friend" | "colleague"

# record
type Person record {
    name string
    age     int
    height  float
    hobbies []string
    circle  [=]string Relation
}

# to consider
# type unions
# * nilability
```

## redirection operations

consider the follwing operations:

```
command-writing-logs > log.txt

jq < my-file.json
```

how do they translate?

as builtins they risk loosing their streaminess

`<` and `>` are not intended to be reserved symbols

```
command-writing-logs |> log.txt

jq <| my-file.json
```

maybe the above are fine for now
