module Sequence.Parsing.Parser
  ( ParseState(..)
  , parseStateRest 
  , ParserT(..)
  , Parser
  , runParser
  , runParserT
  , hoistParserT
  , mapParserT
  , consume
  , position
  , fail
  , failWithPosition
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (defer, class Lazy)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT, mapExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, gets, mapStateT, modify_, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class Alternative, class MonadZero, class MonadPlus, class Plus)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, over, un)
import Data.Tuple (Tuple(..))
import Sequence.Parsing.Parser.Class (class Parsable, rep)
import Sequence.Parsing.Parser.ParseError (ParseError(..))
import Utils.Data.Newtype (through)


-- | Contains the remaining input and current position.
newtype ParseState rep pos = ParseState {
                               rest :: rep,
                               pos :: pos,
                               consumed :: Boolean }

derive instance parseStateNewtype :: Newtype (ParseState rep pos) _

parseStateRest :: forall rep pos. ParseState rep pos -> rep
parseStateRest = through ParseState _.rest

-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either `String`,
-- | or some sort of token stream.
newtype ParserT rep pos m a = ParserT (ExceptT (ParseError pos) (StateT (ParseState rep pos) m) a)

derive instance newtypeParserT :: Newtype (ParserT rep pos m a) _

-- | Apply a parser, keeping only the parsed result.
runParserT :: forall seq elem rep pos m a.
              Parsable seq elem rep pos => Monad m =>
              seq -> ParserT rep pos m a -> m (Either (ParseError pos) a)
runParserT s p = evalStateT (runExceptT (un ParserT p)) $ ParseState { rest: rep s, pos: def, consumed: false }


-- | The `Parser` monad is a synonym for the ParserT monad transformer applied to the `Identity` monad.
type Parser rep pos = ParserT rep pos Identity

-- | Apply a parser, keeping only the parsed result.
runParser :: forall seq elem rep pos a. Parsable seq elem rep pos =>
             seq -> Parser rep pos a -> Either (ParseError pos) a
runParser s = un Identity <<< runParserT s

hoistParserT :: forall seq elem rep pos m n a.
                Parsable seq elem rep pos =>
                (m ~> n) -> ParserT rep pos m a -> ParserT rep pos n a
hoistParserT = mapParserT

-- | Change the underlying monad action and data type in a ParserT monad action.
mapParserT :: forall seq elem rep pos m a n b.
              Parsable seq elem rep pos =>
              ( m (Tuple (Either (ParseError pos) a) (ParseState rep pos)) ->
                n (Tuple (Either (ParseError pos) b) (ParseState rep pos)) ) ->
              ParserT rep pos m a -> ParserT rep pos n b
mapParserT = over ParserT <<< mapExceptT <<< mapStateT

instance lazyParserT :: Lazy (ParserT rep pos m a) where
  defer f = ParserT <<< ExceptT <<< defer $ runExceptT <<< un ParserT <<< f

instance semigroupParserT :: (Monad m, Semigroup a) => Semigroup (ParserT rep pos m a) where
  append = lift2 (<>)

instance monoidParserT :: (Monad m, Monoid a) => Monoid (ParserT rep pos m a) where
  mempty = pure mempty

derive newtype instance functorParserT :: Functor m => Functor (ParserT rep pos m)
derive newtype instance applyParserT :: Monad m => Apply (ParserT rep pos m)
derive newtype instance applicativeParserT :: Monad m => Applicative (ParserT rep pos m)
derive newtype instance bindParserT :: Monad m => Bind (ParserT rep pos m)
derive newtype instance monadParserT :: Monad m => Monad (ParserT rep pos m)
derive newtype instance monadRecParserT :: MonadRec m => MonadRec (ParserT rep pos m)
derive newtype instance monadStateParserT :: Monad m => MonadState (ParseState rep pos) (ParserT rep pos m)
derive newtype instance monadThrowParserT :: Monad m => MonadThrow (ParseError pos) (ParserT rep pos m)
derive newtype instance monadErrorParserT :: Monad m => MonadError (ParseError pos) (ParserT rep pos m)

instance altParserT :: Monad m => Alt (ParserT rep pos m) where
  alt p1 p2 = ParserT <<< ExceptT <<< StateT $ \s -> do
    let clean = over ParseState _ { consumed = false } s
    result@(Tuple e s') <- runStateT (runExceptT <<< un ParserT $ p1) clean
    case e of
      Left err
        | false <- through ParseState _.consumed s' -> runStateT (runExceptT <<< un ParserT $ p2) s
      _                                             -> pure result

instance plusParserT :: Monad m => Plus (ParserT rep pos m) where
  empty = fail "No alternative"

instance alternativeParserT :: Monad m => Alternative (ParserT rep pos m)

instance monadZeroParserT :: Monad m => MonadZero (ParserT rep pos m)

instance monadPlusParserT :: Monad m => MonadPlus (ParserT rep pos m)

instance monadTransParserT :: MonadTrans (ParserT s pos) where
  lift = ParserT <<< lift <<< lift

-- | Set the consumed flag.
consume :: forall rep pos m. Monad m => ParserT rep pos m Unit
consume = modify_ $ over ParseState _{ consumed = true }

-- | Returns the current position in the stream.
position :: forall rep pos m. Monad m => ParserT rep pos m pos
position = gets $ through ParseState _.pos

-- | Fail with a message.
fail :: forall rep pos m a. Monad m => String -> ParserT rep pos m a
fail msg = position >>= failWithPosition msg

-- | Fail with a message and a position.
failWithPosition :: forall rep pos m a. Monad m => String -> pos -> ParserT rep pos m a
failWithPosition msg pos = throwError <<< ParseError $ { msg, pos }

