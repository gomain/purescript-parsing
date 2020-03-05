module Sequence.Parsing.Parser.Class (
  class SequenceRep,
  rep,
  null,
  class Sequence,
  uncons,
  class Drop,
  drop,
  class Update,
  update,
  class Parsable ) where

import Data.Default (class Default)
import Data.Maybe (Maybe)

class SequenceRep seq rep | seq -> rep, rep -> seq where
  rep :: seq -> rep

class Sequence seq elem | seq -> elem where
  uncons :: seq -> Maybe { head :: elem, tail :: seq }
  null :: seq -> Boolean
  
class Drop with rep where
  drop :: with -> rep -> Maybe rep

class Update with pos where
  update :: with -> pos -> pos
  
class (SequenceRep seq rep,
       Sequence seq elem,
       Sequence rep elem,
       Drop seq rep,
       Drop elem rep,
       Default pos,
       Update seq pos,
       Update elem pos) <=
       Parsable seq elem rep pos | seq -> elem rep pos, rep -> seq
