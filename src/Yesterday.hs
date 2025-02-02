module Yesterday where

import qualified Data.Text as T

import Yesterday.Types
import Yesterday.Parser

parseDirectory :: T.Text -> IO Function
parseDirectory _ = do
  -- TODO: Actually iterate the whole directory for parsing
  parseFile "Split.time" (parseFunction "Split.time")
