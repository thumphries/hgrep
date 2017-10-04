{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Data.ByteString.Char8 as B8

import qualified Language.Haskell.HGrep as HGrep
import           Language.Haskell.HGrep.Prelude

import qualified Options.Applicative as O

import qualified System.Console.ANSI as ANSI
import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO

import qualified Text.Regex.PCRE.Heavy as PCRE


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  opts <- parseOpts
  colour <- detectColour
  case parseQuery (cmdQuery opts) (cmdRegex opts) of
    Left er -> do
      IO.hPutStrLn IO.stderr er
      exitWith (ExitFailure 2)
    Right q -> do
      found <-
        fmap sum $
          for (cmdFiles opts) $ \fp ->
            hgrep (HGrep.PrintOpts colour) q fp
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
    Left err -> do
      IO.hPutStrLn IO.stderr (HGrep.printParseError err)
      pure 0

    Right src -> do
      let results = HGrep.queryModule q src
      HGrep.printResults popts results
      pure $ fromIntegral (length results)


parseQuery :: [Char] -> Bool -> Either [Char] HGrep.Query
parseQuery str regex =
  case regex of
    True ->
      HGrep.MatchRegex <$> PCRE.compileM (B8.pack str) []
    False ->
      pure $ HGrep.MatchSimple str

-- -----------------------------------------------------------------------------

data CmdOpts = CmdOpts {
    cmdQuery :: [Char]
  , cmdRegex :: Bool
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
    <*> O.switch
          (O.short 'e' <> O.long "regex" <> O.help "Match a regular expression")
    <*> many (O.argument O.str (O.metavar "FILE"))
