module Sequence.Parsing.Parser.Class (
  class Represent,
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

class Represent tee tive | tee -> tive where
  rep :: tee -> tive

class Sequence seq elem | seq -> elem where
  uncons :: seq -> Maybe { head :: elem, tail :: seq }
  null :: seq -> Boolean
  
class Drop with dropable where
  drop :: with -> dropable -> Maybe dropable

class Update with updatable where
  update :: with -> updatable -> updatable
  
class (Represent seq rep,
       Sequence rep elem,
       Drop seq rep,
       Drop elem rep,
       Default pos,
       Update seq pos,
       Update elem pos) <=
       Parsable seq elem rep pos | seq -> elem rep pos
