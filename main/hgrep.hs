{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified HGrep as HGrep
import           HGrep.Prelude

import qualified Options.Applicative as O


main :: IO ()
main = do
  opts <- parseOpts
  for_ (cmdGlobs opts) $ \fp ->
    HGrep.grepFile fp (cmdQuery opts)

-- -----------------------------------------------------------------------------

data CmdOpts = CmdOpts {
    cmdQuery :: [Char]
  , cmdGlobs :: [FilePath]
  } deriving (Eq, Ord, Show)

parseOpts :: IO CmdOpts
parseOpts =
  O.execParser $
    O.info
      (parser <**> O.helper)
      (O.header "hgrep - search Haskell source code from the command line")

parser :: O.Parser CmdOpts
parser =
  CmdOpts
    <$> O.argument O.str (O.metavar "QUERY")
    <*> many (O.argument O.str (O.metavar "GLOB"))
