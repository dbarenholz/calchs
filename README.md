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
- [ ] Flags, e.g. for scientific mode.
- [ ] Interactive mode. To make this, I need to do some administration (for history), as well as handle arrow key inputs.
- [ ] Functions: `floor`, `ceil`, `min`, `max`, .... This will introduce a new Literal type, which then needs to be correctly lexed, parsed, and evaluated.
- [ ] List types. With list types, one can write `min(1, 2, 3, 4, 5)`, and it will return `1`. Implementing list types is involed in parsing (handle comma's) and evaluation, as the Result type will change.

## Building

This project uses _Cabal_.
If you don't have Cabal or GHC yet, I suggest installing them through [ghcup](https://www.haskell.org/ghcup/).

Build the project using `cabal build`. Then, use `cabal list-bin` to find where cabal put the executable.

## Testing

To test the project, you have 4 options:

1. Test the entire application: `cabal test --show-details=always`. This will internally call `cabal run` on the project, and compare its output to the testcase.
2. Test the lexer: `cabal test --test-show-details=always --test-options=lexer`. This internally directly tests the `lex` function. 
3. Test the parser: `cabal test --test-show-details=always --test-options=parser`. This internally directly tests the `parse` function.
4. Test the evaluator: `cabal test --test-show-details=always --test-options=evaluator`. This internally directly tests the `eval` function.

## Contributing

Since this is a learning project, the only contributions I'll accept are _ideas_! 
Feel free to make an issue if you think something is interesting to add, either because you want to use it yourself, or because you believe it's a good learning experience.
