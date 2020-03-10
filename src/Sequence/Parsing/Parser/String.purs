-- | Primitive parsers for working with an input stream of type `String`.

module Sequence.Parsing.Parser.String where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (many)
import Data.Enum (fromEnum)
import Data.Foldable (elem, notElem)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Sequence.Parsing.Parser (ParserT, fail)
import Sequence.Parsing.Parser.Class (IndexedSequenceRep, StringPos)
import Sequence.Parsing.Parser.Combinators ((<?>))
import Sequence.Parsing.Parser.Primitives as P

type StringParserT m a = ParserT String (IndexedSequenceRep (Array CodePoint)) StringPos m a

-- | Match end-of-file.
eof :: forall m. Monad m => StringParserT m Unit
eof = P.end <?> "Expected EOF"

-- | Match the specified string.
string :: forall m. Monad m => String -> StringParserT m String
string str = P.string str <?> ("Expected " <> show str)

-- | Match any code point.
anyCodePoint :: forall m. Monad m => StringParserT m CodePoint
anyCodePoint = P.any <?> "Unexpected EOF"

-- | Match a code point satisfying the specified predicate.
satisfy :: forall m. Monad m => (CodePoint -> Boolean) -> StringParserT m CodePoint
satisfy pred = P.when pred
               <|> failShowCodePoint "does not satisfy predicate"

-- | Match the specified code point
codePoint :: forall m. Monad m => CodePoint -> StringParserT m CodePoint
codePoint cp = P.match cp <|> failShowCodePoint ("does not match " <> show cp)

-- | Match zero or more whitespace.
whiteSpace :: forall m. Monad m => StringParserT m String
whiteSpace = CP.fromCodePointArray <$> many (satisfy isWhiteSpace) where
  isWhiteSpace :: CodePoint -> Boolean
  isWhiteSpace = flip elem whiteSpaces <<< fromEnum where
    -- https://en.wikipedia.org/wiki/Whitespace_character
    whiteSpaces = [9,10,11,12,13,32,133,160,5760,8192,8193,8194,8195,8196,8197,
                   8198,8199,8200,8201,8202,8232,8233,8239,8287,12288]

-- | Skip whitespace characters.
skipSpaces :: forall m. Monad m => StringParserT m Unit
skipSpaces = void whiteSpace

-- | Match one of the characters in the array.
oneOf :: forall m. Monad m => Array CodePoint -> StringParserT m CodePoint
oneOf cps = satisfy (flip elem cps)
            <|> failShowCodePoint ("is not one of " <> show cps)

-- | Match any character not in the array.
noneOf :: forall m. Monad m => Array CodePoint -> StringParserT m CodePoint
noneOf cps = satisfy (flip notElem cps)
             <|> failShowCodePoint ("is none of " <> show cps)

failShowCodePoint :: forall m a. Monad m => String -> StringParserT m a
failShowCodePoint msg = anyCodePoint >>= \cp -> fail $ "'" <> show cp <> "' " <> msg
