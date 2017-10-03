# hgrep

Search Haskell source code from the command line.

Powered by ghc-exactprint.

## Usage

`hgrep` requires an expression and a set of files to search across to
function. For now, an expression can only be the full name of a type
or an expression.

```
$> hgrep
Usage: hgrep EXPRESSION [FILE]
```

```
$> hgrep main main/hgrep.hs
main/hgrep.hs:16:1-13

-- | Run the program.
main :: IO ()
main/hgrep.hs:(17,1)-(18:27)

main = do
  putStrLn "Hello, world!"
```

```
$> hgrep PrintOpts src/**/*.hs
src/Language/Haskell/HGrep/Internal/Data.hs:(40,1)-(42,28)

data PrintOpts = PrintOpts {
    poColourOpts :: ColourOpts
  } deriving (Eq, Ord, Show)
```
