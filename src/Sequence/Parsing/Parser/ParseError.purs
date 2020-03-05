module Sequence.Parsing.Parser.ParseError (
  ParseError(..),
  parseErrorMessage,
  parseErrorPosition ) where

import Data.Newtype (class Newtype)
import Prelude (class Show, show, (<<<), (<>))
import Utils.Data.Newtype (through)

-- | A parsing error, consisting of a message and position information.
newtype ParseError pos = ParseError { msg :: String, pos :: pos }

derive instance parseErrorNewtype :: Newtype (ParseError pos) _

parseErrorMessage :: forall pos. ParseError pos -> String
parseErrorMessage = through ParseError _.msg

parseErrorPosition :: forall pos. ParseError pos -> pos
parseErrorPosition  = through ParseError _.pos 

instance showParseError :: Show pos => Show (ParseError pos) where
  show = ("ParseError " <> _) <<< through ParseError show

--derive instance eqParseError :: Eq ParseError
--derive instance ordParseError :: Ord ParseError
