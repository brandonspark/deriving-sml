# deriving-sml

`deriving-sml` is a tool for type-generated code generation in Standard ML,
similar to `ppx_deriving` for OCaml, and deriving traits in Rust.

It currently supports four plugins: `show`, `eq`, `compare`, and `map`.

`deriving-sml` is intended to be used for SML projects using the Standard ML of
New Jersey compiler.

`deriving-sml` is implemented in Standard ML, as all things should be.

## Usage

`deriving-sml` is triggered in a similar way to `ppx_deriving` for OCaml.

After a (data)type declaration or specification, you can write `[.deriving
<plugins>]` to generate code for the attached type.

```sml
signature FOO =
  sig
    type t [.deriving show]

    datatype t2 =
        One
      | Two [.deriving show, compare]
  end

structure Foo : FOO =
  struct
    type t = int * string [.deriving show, eq]

    datatype t2 =
        One
      | Two [.deriving show, compare]
  end
```

For a given plugin, such as `show`, when derived for a type `t`, it will
generate functions `t_show` and `show_t` within the same scope.

Similarly to how one might run `sml <filename_1> ... <filename_n>` to load a
certain number of dependencies in to the SML/NJ REPL, you run `deriving-sml` by
using the enclosed `./run` command as `./run <filename_1> ... <filename_n>`.

This will cause all involved files (even through CM dependencies) to be
rewritten with all derived code, and restored upon termination of the REPL. This
means that `deriving-sml` works "invisibly", and `./run` is meant to function
identically to `sml`.

## Examples

Suppose we had the following code:

```sml
datatype 'a t = A | B of 'a [.deriving show, eq, compare, map]
```

Then this will generate code like:
```sml
val show_t : ('a -> string) -> 'a t -> string
val eq_t : ('a * 'a -> bool) -> 'a t * 'a t -> bool
val compare_t : ('a * 'a -> order) -> 'a t * 'a t -> order
val map_t : ('a -> 'b) -> 'a t -> 'b t
```

There are a fair amount of tests included in `test/test.sml`. You can run them
by running `./run test/sources.cm` and then `Test.run_tests ()`.

## CM Compatibility

`deriving-sml` is meant to be able to be used to enable deriving code anywhere
within a given SML project. As such, it is fully CM compatible - you can run
`./run sources.cm` and cause it to rewrite all of the dependencies of
`sources.cm` (even if it is other `.cm` files).

## Polymorphism and Derive Expressions

`deriving-sml` also permits _derive expressions_, which are triggered via expressions of the
form `[.<plugin>: <type>]`. These will generate code for the corresponding
types, similar to above.

Notably, these types may contain type variables in them. This will generate a
function expecting curried arguments for those types. The order of the curried arguments is in sorted order of the involved type variables.

So, for instance, `[.map: ('b * 'a option) list list] : ('a -> 'a2) -> ('b ->
'b2) -> ('b * 'a option) list list -> ('b2 * 'a2 option) list list`.

