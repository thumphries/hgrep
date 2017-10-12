{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Language.Haskell.HGrep as HGrep
import qualified Language.Haskell.HGrep.Internal.Data as HGrep
import           Language.Haskell.HGrep.Prelude

import qualified Options.Applicative as O

import qualified System.Console.ANSI as ANSI
import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO
import qualified System.Directory as D
import qualified System.FilePath as FP


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
      -- if no directory provided -> search in current directory
      files <-
        case cmdFiles opts of
          [] -> pure <$> D.getCurrentDirectory
          x  -> pure x
      allFiles <- foldMap getAllHsFiles files
      found <-
        fmap sum $
          for allFiles $ \fp -> do
            hgrep (HGrep.PrintOpts colour (cmdLineNums opts)) q fp
      exitWith (exitCode found)

getAllHsFiles :: FilePath -> IO [FilePath]
getAllHsFiles fp = do
  isDir <- D.doesDirectoryExist fp
  if isDir
  then do
    fs <- fmap (fp FP.</>) <$> D.listDirectory fp
    foldMap getAllHsFiles fs
  else pure $
    case FP.takeExtension fp of
      ".hs"  -> [fp]
      ".lhs" -> [fp]
      _      -> []

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
      HGrep.MatchRegex <$> HGrep.compileRegex str
    False ->
      pure $ HGrep.MatchSimple str

-- -----------------------------------------------------------------------------

data CmdOpts = CmdOpts {
    cmdQuery    :: [Char]
  , cmdRegex    :: Bool
  , cmdFiles    :: [FilePath]
  , cmdLineNums :: HGrep.LineNumOpts
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
    <*> O.flag HGrep.PrintLineNums HGrep.NoLineNums
          (O.short 'n' <> O.long "no-numbers" <> O.help "Turn line numbering off")
