## calchs

A calculator language, implemented in Haskell.
This project has three main reasons for existing.

1. I want a command line tool to quickly calculate things.
2. I want a usecase for Haskell, because I'm learning the language.
3. I'm interested in how parsers work.

### Features

`calchs` supports the following features, subject to change:

1. Supports numbers, differentiating between `Int`s and `Float`s.
2. Supports unary operations:
    1. negation: `-1`
3. Supports binary operations:
    1. addition: `1 + 1`
    2. subtraction: `1 - 1`
    3. multiplication: `1 * 1`
    4. division: `1 / 1`
    5. power: `1 ^ 1`
4. Supports arbitrary (matched) parenthesized expressions; running `calchs "(((1))) + ((((-1))))"` prints `0`.
5. Usable in scripts. Running `calchs "1 + $(calchs "1+1")"` will print `3`.
6. Usable as interactive session. Running `calchs` (with no arguments) will spawn an interactive session where you can do all sorts of fun math.
7. Supports certain options. See [the options](#options) for details.


### Options

`calchs` supports the following options, subject to change:

* `--help` (or `-h`): shows help for the program
* `--version`: shows the program version

See [the roadmap](#roadmap) for options that are parsed, but not yet implemented.

### Building

`calchs` uses [`stack`](<https://docs.haskellstack.org/en/stable/>) for building.
If you don't have it, install it with [ghcup](https://www.haskell.org/ghcup/).

To build `calchs`, clone it to a desired location on your machine.
When you `cd calchs` into the directory, `stack` will install the required `GHC` version.
When installing is done, run `stack build` to build the project.
Install it with `stack install`.

### Testing

`calchs` uses [`hspec`](<https://hspec.github.io>) with [automatic spec discovery](<https://hspec.github.io/hspec-discover.html>).

- To test the entire application: `stack test`.
- To test a specific part of tests: `stack test --test-arguments '-m "PART"'`, where `PART` is a (sub)string of the tests you want to run.

### Roadmap

This describes the current state of the project, and my plans for it.

#### Options

* `--joke`: enables the joke mode
* `--imprecise`: enables an imprecise mode
* `--cats`: emulate cats walking over your keyboard in interactive mode
* `--scientific`: enables scientific notation for results
* `--convert`: enable conversion mode
* `--mode [b(inary) | h(ex) | 1234]`: sets binary (`b` and `binary` work), hex (`h` and `hex` work), or arbitary base mode by passing an `Int`.

> Note that the last option takes precedence.
> Running `calchs --mode h --mode b --mode 10`, we silently ignore `--mode h` and `--mode b`, and simply set the mode to base 10 (which is default).


#### Features

- [ ] Functions: `floor`, `ceil`, `min`, `max`, .... This will introduce a new Literal type, which then needs to be correctly lexed, parsed, and evaluated.
	- [ ] Eventually: figure out how to auto-complete words (e.g. `floor`) when pressing `Tab`.
- [ ] List types. With list types, one can write `min(1, 2, 3, 4, 5)`, and it will return `1`. Implementing list types is involed in parsing (handle comma's) and evaluation, as the Result type will change. _Note: notation not final._
  - [ ] When we have lists (vectors), we can implement operations for those e.g. scalar multiplication `*`, the dot product `.`, the cross product `x`, ...
- [ ] Conversions
    - [ ] Between different units of length, mass, and others.
    - [ ] Between different number representations (e.g. binary, octal, hex).

#### Testing

- [ ] Test for underflows and overflows
- [ ] Test for parsing options accurately
- [ ] Test for _using_ options accurately

### Contributing

Since this is a learning project, the only contributions I'll accept are _ideas_!
Feel free to make an issue if you think something is interesting to add, either because you want to use it yourself, or because you believe it's a good learning experience.
