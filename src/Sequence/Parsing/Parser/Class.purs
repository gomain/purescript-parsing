module Sequence.Parsing.Parser.Class (
  class Represent,
  rep,
  IndexedSequenceRep,
  class Uncons,
  uncons,
  null,
  class StripPrefix,
  stripPrefix,
  class Update,
  update,
  class Parsable,
  StringPos,
  Index ) where

import Control.MonadPlus (guard)
import Data.Array as A
import Data.Default (class Default)
import Data.Foldable (class Foldable, foldl)
import Data.Foldable as F
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe, isNothing)
import Data.Newtype (class Newtype, over, un)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Prelude (class Eq, bind, discard, flip, identity, mod, pure, ($), (+), (-), (<<<), (==))

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

class Uncons seq elem | seq -> elem where
  uncons :: seq -> Maybe { head :: elem, tail :: seq }

null :: forall seq elem. Uncons seq elem => seq -> Boolean
null = isNothing <<< uncons

instance sequenceIndexedSequenceRepArray :: Uncons (IndexedSequenceRep (Array a)) a where
  uncons rep = do
    let { seq, index } = un IndexedSequenceRep rep
    head <- seq `A.index` index
    pure { head, tail: IndexedSequenceRep { seq, index: index + 1 } }

instance sequenceList :: Uncons (List a) a where
  uncons = L.uncons
  
class StripPrefix prefix sequence where
  stripPrefix :: prefix -> sequence -> Maybe sequence

instance dropStringIndexedSequenceArrayCodePoint :: StripPrefix String (IndexedSequenceRep (Array CodePoint)) where
  stripPrefix str = stripPrefix $ CP.toCodePointArray str
else
instance dropArrayAIndexedSequenceArrayA :: Eq a => StripPrefix (Array a) (IndexedSequenceRep (Array a)) where
  stripPrefix arr rep = do
    let { seq, index } = un IndexedSequenceRep rep
        end = index + A.length arr
    guard $ arr == A.slice index end seq
    pure $ over IndexedSequenceRep _ { index = end } rep
else
instance dropAIndexedSequenceArrayA :: Eq a => StripPrefix a (IndexedSequenceRep (Array a)) where
  stripPrefix = dropMatchingHead_ uncons
else
instance dropListAListA :: Eq a => StripPrefix (List a) (List a) where
  stripPrefix = L.stripPrefix <<< L.Pattern
else
instance dropAListA :: Eq a => StripPrefix a (List a) where
  stripPrefix = dropMatchingHead_ uncons

dropMatchingHead_ :: forall a as. Eq a => (as -> Maybe { head :: a, tail :: as }) -> a -> as -> Maybe as
dropMatchingHead_ uncons a as = do
    { head, tail } <- uncons as
    guard $ head == a
    pure tail

class Update with updatable where
  update :: with -> updatable -> updatable

newtype StringPos = StringPos { line :: Int, column :: Int }

derive instance newtypeStringPos :: Newtype StringPos _

instance defaultStringPos :: Default StringPos where
  def = StringPos { line: 1, column: 1 }

instance updateCodePointStringPos :: Update CodePoint StringPos where
  update char (StringPos { line, column }) = StringPos $ case char of
    c | c `A.elem` [newline, return] -> { line: line + 1, column: 1 }
      | c == tab                     -> { line,           column: column + 8 - ((column -1) `mod` 8) }
    _                                -> { line,           column: column + 1 }
    where newline = CP.codePointFromChar '\n'
          return  = CP.codePointFromChar '\r'
          tab     = CP.codePointFromChar '\t'

instance updateStringStringPos :: Update String StringPos where
  update str pos = foldl (flip update) pos (CP.toCodePointArray str)

newtype Index = Index Int

derive instance newtypeIndexedSequencePos :: Newtype Index _

instance defaultIndex :: Default Index where
  def = Index 0

instance updateFoldableAIndex :: Foldable f => Update (f a) Index where
  update f = over Index $ (_ + F.length f)
else
instance updateAIndex :: Update a Index where
  update _ = over Index (_ + 1)

class (Represent seq rep,
       Uncons rep elem,
       StripPrefix seq rep,
       StripPrefix elem rep,
       Default pos,
       Update seq pos,
       Update elem pos) <=
       Parsable seq elem rep pos | seq -> elem rep pos

instance parsableString :: Parsable String CodePoint (IndexedSequenceRep (Array CodePoint)) StringPos

instance parsableArrayA :: Eq a => Parsable (Array a) a (IndexedSequenceRep (Array a)) Index

instance parsableListA :: Eq a => Parsable (List a) a (List a) Index
