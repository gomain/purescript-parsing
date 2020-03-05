module Sequence.Parsing.Parser.Data (
  ParseError(..),
  parseErrorMessage,
  parseErrorPosition,
  ParseState(..),
  parseStateRest ) where

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

-- | Contains the remaining input and current position.
newtype ParseState rep pos = ParseState {
                               rest :: rep,
                               pos :: pos,
                               consumed :: Boolean }

derive instance parseStateNewtype :: Newtype (ParseState rep pos) _

parseStateRest :: forall rep pos. ParseState rep pos -> rep
parseStateRest = through ParseState _.rest
