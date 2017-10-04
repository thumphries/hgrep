{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Language.Haskell.HGrep as HGrep
import           Language.Haskell.HGrep.Prelude

import qualified Options.Applicative as O

import qualified System.Console.ANSI as ANSI
import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  opts <- parseOpts
  colour <- detectColour
  found <-
    fmap sum $
      for (cmdFiles opts) $ \fp ->
        hgrep (HGrep.PrintOpts colour) (cmdQuery opts) fp
  exitWith (exitCode found)

exitCode :: Integer -> ExitCode
exitCode found
  | found == 0 = ExitFailure 1
  | otherwise = ExitSuccess

detectColour :: IO HGrep.ColourOpts
detectColour = do
  out <- ANSI.hSupportsANSI IO.stdout
  err <- ANSI.hSupportsANSI IO.stderr
  pure $ case out && err of
    True ->
      HGrep.DefaultColours
    False ->
      HGrep.NoColours

hgrep :: HGrep.PrintOpts -> HGrep.Query -> FilePath -> IO Integer
hgrep popts q fp = do
  esrc <- HGrep.parseModule fp
  case esrc of
    Left _err ->
      -- FIXME report or count errors
      pure 0

    Right src -> do
      let results = HGrep.queryModule (HGrep.MatchSimple q) src
      HGrep.printResults popts results
      pure $ fromIntegral (length results)

-- -----------------------------------------------------------------------------

data CmdOpts = CmdOpts {
    cmdQuery :: [Char]
  , cmdFiles :: [FilePath]
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
    <*> many (O.argument O.str (O.metavar "FILE"))
