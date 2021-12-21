module Parsers where

import Prelude
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Text.Parsing.Parser (ParseError, Parser, fail, runParser)

parseInt :: NonEmptyList Char -> Parser String Int
parseInt = fromFoldable >>> fromCharArray >>> fromString >>> maybe (fail "Not an int") pure

parseFile :: forall a. FilePath -> Parser String a -> Effect (Either ParseError a)
parseFile filePath parser = do
  file <- flip runParser parser <$> readTextFile UTF8 filePath
  pure file
