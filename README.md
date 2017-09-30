# hgrep

Search Haskell source code from the command line.

Powered by ghc-exactprint and regex.

## Feature map

Don't use this unless you're willing to work on it. Not much is done.

### Predicate Queries required for release
- [X] Find type declarations
- [ ] Find value declarations
- [ ] Find uses of type
- [ ] Find uses of value
- [ ] Regex for all of the above

### Search Queries worth exploring
- [ ] Find all uses of certain syntactic features (e.g. LambdaCase, PatternSynonyms)
- [ ] Find the body of a certain declaration
- [ ] Find publicly-exported things
- [ ] ??? too many possibilities, what is useful?

### CLI filter/predicate syntax

### CLI usage

Mock up, not real.

```
Usage: hgrep [FILTERS] [[-e] EXPRESSION] [FILES]

hgrep fooBarBaz 'src/**/*.hs' "test/Test/**/*.hs" # uses and declarations of fooBarBaz
hgrep FooBarBaz 'src/**/*.hs'                     # uses and declarations of FooBarBaz
hgrep --decls --types --patterns FooBarBaz        # uses of FooBarBaz as a type
hgrep --types --values -e 'Foo$' src/**/*.hs      # uses of types and values matching 'Foo$'
```

when there are no filters or expression, halt with usage.

when there are filters but no expression, proceed.

when there are no files supplied, read a single module from stdin.

exitcodes like grep, namely
- 0     One or more lines were selected.
- 1     No lines were selected.
- >1    An error occurred.

multiple filters are OR'd and run in a consistent order.

globs can be passed through verbatim for more consistent globstar
expansion (is this a good idea?)
