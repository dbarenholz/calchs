<p align="center"><em>this repository is mirrored from my own <a href="https://dbarenholz.hopto.org/gitea/dan/calchs">gitea</a> instance</em></p>

## calchs

A calculator language, implemented in Haskell. This project has three main reasons for existing.

1. I want a command line tool to quickly calculate things.
2. I want a usecase for Haskell, because I'm learning the language.
3. I'm interested in how parsers work.

## Features

1. Supports numbers (duh), and differentiates between `integer`s and `float`s.
2. Supports following unary operations:
    1. negation: `-1`
3. Supports following binary operations:
    1. addition: `1 + 1`
    2. subtraction: `1 - 1`
    3. multiplication: `1 * 1`
    4. division: `1 / 1`
    5. power: `1 ^ 1`
4. Supports arbitrary (matched) parenthesized expressions; `(((1))) + ((((-1))))` is `0`.
5. Usable in scripts. Running `calchs "1 + $(calchs "1+1")"` will print `3` on `stdin`.
6. WIP: Usable as interactive session. Running `calchs` (with no arguments) will spawn an interactive session where you can do all sorts of fun math.

## Tests

All tests for this project live in a single file in the `test` folder.

I have the following types of tests:

1. Property-based tests: the _roundtrip_ property (running the calculator on a representation of an AST returns the same AST).
2. Table-based tests for happy and unhappy flow.
3. Unit tests for `lex`, `parse`, and `eval` functions (based on happy table).

See also [how to run the tests yourself](#testing).

## Roadmap

Here is a roadmap of things that I still want to do.

**Interactive mode**:

- [ ] Ensure that I'm parsing the right thing for my own terminal, _not_ the one obtained when doing `:term` in nvim.
- [ ] Figure out how to keep track of history for `Up` and `Down`.
- [ ] Properly handle `ALT` keypresses with a timeout.

**Features**:

- [ ] Functions: `floor`, `ceil`, `min`, `max`, .... This will introduce a new Literal type, which then needs to be correctly lexed, parsed, and evaluated.
	- [ ] Eventually: figure out how to auto-complete words (e.g. `floor`) when pressing `Tab`.
- [ ] List types. With list types, one can write `min(1, 2, 3, 4, 5)`, and it will return `1`. Implementing list types is involed in parsing (handle comma's) and evaluation, as the Result type will change.
- [ ] Flags, e.g. for scientific mode.
- [ ] Conversions
    - [ ] Between different units of length, mass, and others.
    - [ ] Between different number representations (e.g. binary, octal, hex).

**Testing**:

- [ ] Test for underflows and overflows

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
