module Parsers where

import Prelude
import Data.Array (fromFoldable)
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser, fail)

parseInt :: NonEmptyList Char -> Parser String Int
parseInt = fromFoldable >>> fromCharArray >>> fromString >>> maybe (fail "Not an int") pure
