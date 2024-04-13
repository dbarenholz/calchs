## calchs

A calculator language, implemented in Haskell. This project has three main reasons for existing.

1. I want a command line tool to quickly calculate things.
2. I want a usecase for Haskell, because I'm learning the language.
3. I'm interested in how parsers work.

## Roadmap

Here is a brief roadmap of my ideas that I still want to do.

- [x] Literal numbers (integers and floats), e.g. `1`, `1.3`, `.2`
- [x] Unary operator: negation.
- [x] The 4 basic binary operators: addition, subtraction, multiplication, division.
- [x] Support parenthesized expressions, e.g. `((1)) + (2)` is valid.
- [x] Testing
    - [x] Property-based tests: the roundtrip property
    - [x] Table-based tests for happy flow
    - [x] Table-based tests for unhappy flow
    - [x] Unit tests (based on same table as happy flow) for individual functions `lex`, `parse` and `eval`
- [ ] Flags, e.g. for scientific mode.
- [ ] Interactive mode. To make this, I need to do some administration (for history), as well as handle arrow key inputs.
- [ ] Functions: `floor`, `ceil`, `min`, `max`, .... This will introduce a new Literal type, which then needs to be correctly lexed, parsed, and evaluated.
- [ ] List types. With list types, one can write `min(1, 2, 3, 4, 5)`, and it will return `1`. Implementing list types is involed in parsing (handle comma's) and evaluation, as the Result type will change.
- [ ] Unit conversion between different types of units (so things like `feet` to `meter`, but also perhaps more esoteric units used in science).


## Building

This project uses _Cabal_.
If you don't have Cabal or GHC yet, I suggest installing them through [ghcup](https://www.haskell.org/ghcup/).

Build the project using `cabal build`. Then, use `cabal list-bin` to find where cabal put the executable.

## Testing

To test the project, you have a few options:

1. Test the entire application: `cabal run test` (or `cabal run test -- "all"`)
2. Test the application using implemented property tests: `cabal run test -- "property"`
3. To replicate a property run using a (positive) seed, e.g. `111`: `cabal run test -- "seed-111"`
4. Test the lexer: `cabal run test -- "lexer"`
5. Test the parser: `cabal run test -- "parser"`
6. Test the evaluator: `cabal run test -- "evaluator"`

If you want to see more (or less) output for a particular, then open `test/Main.hs`, and replace the default `ShowFail` (show all failures and errors) with any valid constructor of the `ShowMe` datatype (copied below)

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
