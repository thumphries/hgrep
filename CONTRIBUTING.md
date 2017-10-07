# Contribution Guidelines

This document contains a few rules of thumb to ensure your changes
will be accepted. It should be really easy to submit a good PR to this
project. Please let me know if the process could be improved.

Often I'll lapse on these to make the process a little easier on
contributors. Follow the spirit, not the letter.

## Be respectful

This is a fairly inconsequential piece of open source software,
written and maintained by volunteers at no charge to you.

With that in mind, please keep all correspondence pleasant,
constructive, and respectful. Repeat offenders will be banned from the
repository.

## Get in touch before chasing geese

If you're planning to do a substantial chunk of work, it might be
worth getting in touch first. Just open an issue and outline what
you're planning to do. Don't surprise maintainers with huge patches.

For smaller chunks and things already spelled out in existing GitHub
issues, don't worry about this! Just go for it.

## Haskell style guidelines

### Language pragmas
Language pragmas go at the top of the module using them:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FooBar where
```

This ensures GHCi and other such tools always work.

Don't add language extensions to the cabal file.

### Imports

Qualified or explicit import lists for Hackage packages.

```haskell
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
```

This makes it much easier to understand your code.

Add padding when `qualified` keyword is not in use.

```haskell
import           Prelude
```

This reduces visual noise and keeps diffs clean.

### Exports 

Explicit export lists always. This helps identify dead code.

```haskell
module Foo (
    Foo
  , parseFoo
  ) where
```

### Formatting

Try to mimic the formatting of the rest of the codebase.

As a general rule, use 2 spaces, plenty of newlines, be sparing with `($)`.
Use Utrecht-style lists with leading commas.
Don't go too overboard with whitespace alignment. Please don't use `hindent`.

This style is intended to be fairly consistent, keeping diffs clean and free
of formatting / indentation adjustments.

```haskell
msg :: [Char]
msg =
  fold [
      "Hello, "
    , printf "%s!\n" "World!"
    ]

foo :: Abc -> Def -> Ghi
foo abc =
  foldM
    (\l r -> traverse (foo l) r)
    abc
    [1..20]

bar ::
     Show xyz
  => AbcDefGhi MonadIO Really Long Type
  -> xyz
  -> IO ()
bar = do
  e <- parseFoo abc
  case e of
    Right abc ->
      pure 123
    Left def ->
      IO.hPutStrLn IO.stderr def

baz :: IO ()
baz =
  for_ [1..10] $ do
    q <- abc (bar baz quux)
    undefined q
```

## Use the project Prelude

This project has its own Prelude module. Use this instead of the usual one.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module FooBar where

import           Project.Name.Prelude
```

The project Prelude exists to identify a common vocabulary, to put useful
and general concepts at your fingertips, and to hide / attach warnings to
unsafe functions from `base`.


Add new things to the Prelude only if
- ... we've talked about it first, or
- ... it exists in `base`, and
- ... you actually need to use it, and
- ... it has an unambiguous name, and
- ... it is safe to use.
    - e.g. `fromJust` is not acceptable, `readMaybe` is

## Don't lint

The only linter that matters is the one configured in CI.

Please don't submit PRs based around fixing things `hlint` identified.

For example, redundant `do` notation hurts nobody.

## Keep the build green

- Do your best to ensure all CI checks pass
- Feel free to ask for help if you're stuck or unsure about this!

## Write tests

If I haven't written any tests, you can probably skip this step. Don't
feel obliged to set up a test framework or anything like that! When there
are tests in place, aspire to include a test or two with each PR.

- Favour property tests and golden tests over unit tests.
- Favour unit tests over nothing at all.
- If it's hard to test, break your code down into smaller, more testable
  functions.
- If it's still hard to test, ask for help.

## Rebase your changes

When PRs fall behind master, they become difficult to merge.

Automatic merging is possible in this scenario, but it means master
will contain code that may not work or even compile. I'll then have to
double back and re-integrate your changes. This is OK sometimes, but
you are in a much better position to do that, since you wrote the code!

- Please rebase onto `origin/master` when your branch falls behind.
    - `git fetch origin; git rebase origin/master`
- Ensure CI passes on your rebased branch.
- If the changes have been approved, they will then be merged.
- If you're struggling with this process, let me know!

## Adhere to the PVP

This project should always be buildable with both Stack and Cabal.
Version bounds should be accurate and PVP-compatible.

Please call me out and open PRs when I've failed at this.

## Don't change the version number

Let me manage the releases. Don't bump the version number yourself.

## Feel free to request a release

Sometimes it's awkward to demand a new Hackage release.

Please feel free to create GitHub issues about this!

## Update the CHANGELOG

Feel free to add your changes to the CHANGELOG.

I may tweak the wording before release.

## Add yourself to CONTRIBUTORS

I'll try to keep this up to date, but feel free to add yourself!

This is not an exclusive list: anyone contributing to any issue or PR
is considered a contributor.

If you'd like to be removed from this list for some reason, please get
in touch privately.
