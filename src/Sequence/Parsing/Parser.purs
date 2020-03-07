module Sequence.Parsing.Parser (
  ParserT(..),
  Parser,
  runParser,
  runParserT,
  hoistParserT,
  mapParserT,
  consume,
  position,
  fail,
  failWithPosition ) where

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
import Data.Default (class Default, def)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, over, un)
import Data.Tuple (Tuple(..))
import Sequence.Parsing.Parser.Class (class Represent, rep)
import Sequence.Parsing.Parser.Data (ParseError(..), ParseState(..))
import Utils.Data.Newtype (through)



-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either `String`,
-- | or some sort of token stream.
newtype ParserT seq rep pos m a = ParserT (ExceptT (ParseError pos) (StateT (ParseState rep pos) m) a)

derive instance newtypeParserT :: Newtype (ParserT seq rep pos m a) _

-- | Apply a parser, keeping only the parsed result.
runParserT :: forall seq rep pos m a.
              Represent seq rep => Default pos => Monad m =>
              seq -> ParserT seq rep pos m a -> m (Either (ParseError pos) a)
runParserT s p = evalStateT (runExceptT (un ParserT p)) $ ParseState { rest: rep s, pos: def, consumed: false }


-- | The `Parser` monad is a synonym for the ParserT monad transformer applied to the `Identity` monad.
type Parser seq rep pos = ParserT seq rep pos Identity

-- | Apply a parser, keeping only the parsed result.
runParser :: forall seq rep pos a.
             Represent seq rep => Default pos =>
             seq -> Parser seq rep pos a -> Either (ParseError pos) a
runParser s = un Identity <<< runParserT s

hoistParserT :: forall seq rep pos m n a.
                (m ~> n) -> ParserT seq rep pos m a -> ParserT seq rep pos n a
hoistParserT = mapParserT

-- | Change the underlying monad action and data type in a ParserT monad action.
mapParserT :: forall seq rep pos m a n b.
              ( m (Tuple (Either (ParseError pos) a) (ParseState rep pos)) ->
                n (Tuple (Either (ParseError pos) b) (ParseState rep pos)) ) ->
              ParserT seq rep pos m a -> ParserT seq rep pos n b
mapParserT = over ParserT <<< mapExceptT <<< mapStateT

instance lazyParserT :: Lazy (ParserT seq rep pos m a) where
  defer f = ParserT <<< ExceptT <<< defer $ runExceptT <<< un ParserT <<< f

instance semigroupParserT :: (Monad m, Semigroup a) => Semigroup (ParserT seq rep pos m a) where
  append = lift2 (<>)

instance monoidParserT :: (Monad m, Monoid a) => Monoid (ParserT seq rep pos m a) where
  mempty = pure mempty

derive newtype instance functorParserT :: Functor m => Functor (ParserT seq rep pos m)
derive newtype instance applyParserT :: Monad m => Apply (ParserT seq rep pos m)
derive newtype instance applicativeParserT :: Monad m => Applicative (ParserT seq rep pos m)
derive newtype instance bindParserT :: Monad m => Bind (ParserT seq rep pos m)
derive newtype instance monadParserT :: Monad m => Monad (ParserT seq rep pos m)
derive newtype instance monadRecParserT :: MonadRec m => MonadRec (ParserT seq rep pos m)
derive newtype instance monadStateParserT :: Monad m => MonadState (ParseState rep pos) (ParserT seq rep pos m)
derive newtype instance monadThrowParserT :: Monad m => MonadThrow (ParseError pos) (ParserT seq rep pos m)
derive newtype instance monadErrorParserT :: Monad m => MonadError (ParseError pos) (ParserT seq rep pos m)

instance altParserT :: Monad m => Alt (ParserT seq rep pos m) where
  alt p1 p2 = ParserT <<< ExceptT <<< StateT $ \s -> do
    let clean = over ParseState _ { consumed = false } s
    result@(Tuple e s') <- runStateT (runExceptT <<< un ParserT $ p1) clean
    case e of
      Left err
        | false <- through ParseState _.consumed s' -> runStateT (runExceptT <<< un ParserT $ p2) s
      _                                             -> pure result

instance plusParserT :: Monad m => Plus (ParserT seq rep pos m) where
  empty = fail "No alternative"

instance alternativeParserT :: Monad m => Alternative (ParserT seq rep pos m)

instance monadZeroParserT :: Monad m => MonadZero (ParserT seq rep pos m)

instance monadPlusParserT :: Monad m => MonadPlus (ParserT seq rep pos m)

instance monadTransParserT :: MonadTrans (ParserT seq rep pos) where
  lift = ParserT <<< lift <<< lift

-- | Set the consumed flag.
consume :: forall seq rep pos m. Monad m => ParserT seq rep pos m Unit
consume = modify_ $ over ParseState _{ consumed = true }

-- | Returns the current position in the stream.
position :: forall seq rep pos m. Monad m => ParserT seq rep pos m pos
position = gets $ through ParseState _.pos

-- | Fail with a message.
fail :: forall seq rep pos m a. Monad m => String -> ParserT seq rep pos m a
fail msg = position >>= failWithPosition msg

-- | Fail with a message and a position.
failWithPosition :: forall seq rep pos m a. Monad m => String -> pos -> ParserT seq rep pos m a
failWithPosition msg pos = throwError <<< ParseError $ { msg, pos }

