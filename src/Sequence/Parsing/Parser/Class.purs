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

import Control.MonadPlus (guard)
import Data.Array as A
import Data.Default (class Default)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe, isNothing)
import Data.Newtype (class Newtype, over, un)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Prelude (class Eq, bind, discard, identity, pure, ($), (+), (<<<), (==))

class Represent tee tive | tee -> tive where
  rep :: tee -> tive

newtype IndexedSequenceRep seq = IndexedSequenceRep { seq :: seq, index :: Int }

makeIndexedSequenceRep :: forall seq. seq -> IndexedSequenceRep seq
makeIndexedSequenceRep seq = IndexedSequenceRep { seq, index: 0 }

derive instance newtypeIndexedSequenceRep :: Newtype (IndexedSequenceRep seq) _

instance representString :: Represent String (IndexedSequenceRep (Array CodePoint)) where
  rep = makeIndexedSequenceRep <<< CP.toCodePointArray

instance representArray :: Represent (Array a) (IndexedSequenceRep (Array a)) where
  rep = makeIndexedSequenceRep

instance representList :: Represent (List a) (List a) where
  rep = identity

class Sequence seq elem | seq -> elem where
  uncons :: seq -> Maybe { head :: elem, tail :: seq }

null :: forall seq elem. Sequence seq elem => seq -> Boolean
null = isNothing <<< uncons

instance sequenceIndexedSequenceRepArray :: Sequence (IndexedSequenceRep (Array a)) a where
  uncons rep = do
    let { seq, index } = un IndexedSequenceRep rep
    head <- seq `A.index` index
    pure { head, tail: IndexedSequenceRep { seq, index: index + 1 } }

instance sequenceList :: Sequence (List a) a where
  uncons = L.uncons
  
class Drop with dropable where
  drop :: with -> dropable -> Maybe dropable -- TODO: rename drop => stripPrefix

instance dropStringIndexedSequenceArrayCodePoint :: Drop String (IndexedSequenceRep (Array CodePoint)) where
  drop str = drop $ CP.toCodePointArray str
else
instance dropArrayAIndexedSequenceArrayA :: Eq a => Drop (Array a) (IndexedSequenceRep (Array a)) where
  drop arr rep = do
    let { seq, index } = un IndexedSequenceRep rep
        end = index + A.length arr
    guard $ arr == A.slice index end seq
    pure $ over IndexedSequenceRep _ { index = end } rep
else
instance dropAIndexedSequenceArrayA :: Eq a => Drop a (IndexedSequenceRep (Array a)) where
  drop = dropMatchingHead_ uncons
else
instance dropListAListA :: Eq a => Drop (List a) (List a) where
  drop = L.stripPrefix <<< L.Pattern
else
instance dropAListA :: Eq a => Drop a (List a) where
  drop = dropMatchingHead_ uncons

dropMatchingHead_ :: forall a as. Eq a => (as -> Maybe { head :: a, tail :: as }) -> a -> as ->  Maybe as
dropMatchingHead_ uncons a as = do
    { head, tail } <- uncons as
    guard $ head == a
    pure tail

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
