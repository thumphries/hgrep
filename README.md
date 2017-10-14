# hgrep
[![Travis Build Status](https://travis-ci.org/thumphries/hgrep.svg?branch=master)](https://travis-ci.org/thumphries/hgrep)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/2xmkp3jbt1q078xv/branch/master?svg=true)](https://ci.appveyor.com/project/thumphries/hgrep)
[![Hackage page (downloads and API reference)](http://img.shields.io/hackage/v/hgrep.svg)](http://hackage.haskell.org/package/hgrep)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hgrep.svg)](http://packdeps.haskellers.com/reverse/hgrep)

Search Haskell source code from the command line.

Powered by ghc-exactprint.

## Usage

```haskell
$> hgrep
Usage: hgrep [-e|--regex] EXPRESSION [FILE]
```

`hgrep` requires an expression and a set of files to search across.

An expression can be one of
- The name of a type, e.g. `FooBar`
- The name of an expression, e.g. `foo`
- A regular expression (via the `-e` flag), e.g. `-e Foo$`

Each file will be parsed and searched. Results will be printed to the
console, with syntax highlighting where possible.

### Requirements

You can use `cabal` or `stack` to install, but you may need `libpcre`. In case you don't have it:

#### Ubuntu

```bash
sudo apt update
sudo apt install libpcre3-dev
```

#### Fedora

```
sudo dnf update
sudo dnf install pcre-devel
```

#### NixOS / Nix Package Manager

```
nix-env -i pcre
```

### Install

#### Stack

```
git clone https://github.com/thumphries/hgrep.git
cd !$
stack install
```


#### Cabal

```
git clone https://github.com/thumphries/hgrep.git
cd !$
cabal new-build
```

#### Mafia

```
git clone https://github.com/thumphries/hgrep.git
cd !$
mafia build
```

#### Nix
```
git clone https://github.com/thumphries/hgrep.git
cd !$
nix-build -E 'with import <nixpkgs> {}; haskellPackages.callCabal2nix "hgrep" ./. {}'
```

### Searching for top-level expressions

```haskell
$> hgrep main main/hgrep.hs
main/hgrep.hs:16:1-13

-- | Run the program.
main :: IO ()
main/hgrep.hs:(17,1)-(18:27)

main = do
  putStrLn "Hello, world!"
```

### Searching for type declarations

```haskell
$> hgrep PrintOpts src/**/*.hs
src/Language/Haskell/HGrep/Internal/Data.hs:(40,1)-(42,28)

data PrintOpts = PrintOpts {
    poColourOpts :: ColourOpts
  } deriving (Eq, Ord, Show)
```

### Searching with a regular expression

```haskell
$> hgrep -e 'Opts$' src/**/*.hs
src/Language/Haskell/HGrep/Internal/Data.hs:(57,1)-(59,28)

data PrintOpts = PrintOpts {
    poColourOpts :: ColourOpts
  } deriving (Eq, Ord, Show)
src/Language/Haskell/HGrep/Internal/Data.hs:(61,1)-(64,26)

defaultPrintOpts :: PrintOpts
src/Language/Haskell/HGrep/Internal/Data.hs:(67,1)-(70,5)

defaultPrintOpts =
  PrintOpts {
      poColourOpts = DefaultColours
    }
```
