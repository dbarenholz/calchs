<p align="center"><em>this repository is mirrored from my own <a href="https://dbarenholz.hopto.org/gitea/dan/calchs">gitea</a> instance</em></p>

## calchs

A calculator language, implemented in Haskell. This project has three main reasons for existing.

1. I want a command line tool to quickly calculate things.
2. I want a usecase for Haskell, because I'm learning the language.
3. I'm interested in how parsers work.

## Features

These are the current features that `calchs` supports. This list will inevitably change in the future.

1. Supports numbers, differentiating between `Int`s and `Float`s.
2. Supports unary operations:
    1. negation: `-1`
3. Supports binary operations:
    1. addition: `1 + 1`
    2. subtraction: `1 - 1`
    3. multiplication: `1 * 1`
    4. division: `1 / 1`
    5. power: `1 ^ 1`
4. Supports arbitrary (matched) parenthesized expressions; running `calchs "(((1))) + ((((-1))))"` returns `0`.
5. Usable in scripts. Running `calchs "1 + $(calchs "1+1")"` will return `3` on `stdin`.
6. Usable as interactive session. Running `calchs` (with no arguments) will spawn an interactive session where you can do all sorts of fun math.
7. Supports certain options. See [the options](#options) for details.


## Options

These are the options that `calchs` currently supports.

* `--help` (or `-h`): shows help for the program
* `--version`: shows the program version


See also [the roadmap](#roadmap) for options that are parsed, but not yet implemented.

## Tests

All tests for this project live in a single file in the `test` folder.

I have the following types of tests:

1. Property-based tests: the _roundtrip_ property (running the calculator on a representation of an AST returns the same AST).
2. Table-based tests for happy and unhappy flow.
3. Unit tests for `lex`, `parse`, and `eval` functions (based on happy table).

See also [how to run the tests yourself](#testing).

## Roadmap

**Options**:

* `--joke`: enables the joke mode
* `--imprecise`: enables an imprecise mode
* `--cats`: emulate cats walking over your keyboard in interactive mode
* `--scientific`: enables scientific notation for results
* `--convert`: enable conversion mode
* `--mode [b(inary) | h(ex) | 1234]`: sets binary (`b` and `binary` work), hex (`h` and `hex` work), or arbitary base mode by passing an `Int`.

> Note that the last option takes precedence.
> Running `calchs --mode h --mode b --mode 10`, we silently ignore `--mode h` and `--mode b`, and simply set the mode to base 10 (which is default).


**Features**:

- [ ] Functions: `floor`, `ceil`, `min`, `max`, .... This will introduce a new Literal type, which then needs to be correctly lexed, parsed, and evaluated.
	- [ ] Eventually: figure out how to auto-complete words (e.g. `floor`) when pressing `Tab`.
- [ ] List types. With list types, one can write `min(1, 2, 3, 4, 5)`, and it will return `1`. Implementing list types is involed in parsing (handle comma's) and evaluation, as the Result type will change. _Note: notation not final._
  - [ ] When we have lists (vectors), we can implement operations for those e.g. scalar multiplication `*`, the dot product `.`, the cross product `x`, ...
- [ ] Conversions
    - [ ] Between different units of length, mass, and others.
    - [ ] Between different number representations (e.g. binary, octal, hex).

**Testing**:

- [ ] Test for underflows and overflows
- [ ] Test for parsing options accurately
- [ ] Test for _using_ options accurately

## Building

This project uses _Cabal_.
If you don't have _Cabal_ or _GHC_ yet, I suggest installing them through [ghcup](https://www.haskell.org/ghcup/).

Build the project using `cabal build`.
Then, use `cabal list-bin` to find where cabal put the executable.

## Testing

To test the project, you have a few options:

1. Test the entire application: `cabal run test` (or `cabal run test -- "all"`)
2. Test the application using implemented property tests: `cabal run test -- "property"`
3. To replicate a property run using a (positive) seed, e.g. `111`: `cabal run test -- "seed-111"`
4. Test the lexer: `cabal run test -- "lexer"`
5. Test the parser: `cabal run test -- "parser"`
6. Test the evaluator: `cabal run test -- "evaluator"`

If you want to see more (or less) output, then replace `ShowFail` with any valid constructor of the `ShowMe` datatype (copied below) in `test/Main.hs`.
Note that showing all `1000` property tests may be unhelpful; I recommend keeping that on `ShowFail`.

```hs
-- | Sum type for showing results.
data ShowMe
  = ShowAll    -- show complete test output
  | ShowFail   -- show all failures and errors <-- default
  | ShowErr    -- show all errors
  | ShowNone   -- show nothing, only the summary
```

## Contributing

Since this is a learning project, the only contributions I'll accept are _ideas_!
Feel free to make an issue if you think something is interesting to add, either because you want to use it yourself, or because you believe it's a good learning experience.
