module Main where

import Options.Applicative
import Yesterday

data Input = File FilePath | StdIn
  deriving (Show)

data Opts = Opts
  { _dirName :: FilePath,
    _mainFile :: Input,
    _yesterdayOpts :: YesterdayOpts,
    _args :: [String]
  }
  deriving (Show)

parseOpts :: Parser Opts
parseOpts =
  let parseFile =
        File
          <$> strOption
            ( long "main"
                <> short 'm'
                <> metavar "SRC"
                <> help "the main function"
            )
      parseStdIn =
        flag'
          StdIn
          ( long "stdin"
              <> help "read from stdin"
          )
   in Opts
        <$> strOption
          ( long "src-directory"
              <> short 's'
              <> help "source directory for library functions"
          )
        <*> (parseFile <|> parseStdIn)
        <*> ( YesterdayOpts
                <$> flag
                  False
                  True
                  ( long "debug"
                      <> short 'd'
                      <> help "enable debug logging"
                  )
            )
        <*> many (argument str (metavar "ARG"))

run :: Opts -> IO ()
run (Opts libDir src opts args) =
  let mainFile = case src of
        StdIn -> "/dev/stdin"
        File fname -> fname
   in mainYesterday opts libDir mainFile args

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (parseOpts <**> helper)
        ( fullDesc
            <> progDesc "compute something yesterday"
        )
