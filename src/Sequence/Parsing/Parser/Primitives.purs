module Sequence.Parsing.Parser.Primitives (
  any,
  takeN,
  when,
  match,
  string,
  end ) where

import Prelude

import Control.Monad.State (gets, put)
import Control.MonadPlus (guard)
import Control.MonadZero (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Sequence.Parsing.Parser (ParserT, consume)
import Sequence.Parsing.Parser.Class (class Parsable, class Sequence, class StripPrefix, class Update, cons, null, rep, singleton, stripPrefix, uncons, update)
import Sequence.Parsing.Parser.Data (ParseState(..), parseStateRest)

any :: forall seq elem rep pos m.
       Parsable seq elem rep pos => Monad m =>
       ParserT seq rep pos m elem
any = do
  { rest, pos } <- gets $ un ParseState
  case uncons rest of
    Nothing -> empty
    Just { head, tail } -> do
      put $ ParseState { rest: tail, pos: update head pos, consumed: true }
      pure head

takeN :: forall seq elem rep pos m.
         Parsable seq elem rep pos => Monad m => Sequence seq elem =>
         Int -> ParserT seq rep pos m seq
takeN n = go n where
  go 0 = singleton <$> any
  go n' = cons <$> any <*> go (n' - 1)

when :: forall seq elem rep pos m.
        Parsable seq elem rep pos => Monad m =>
        (elem -> Boolean) -> ParserT seq rep pos m elem
when pred = do elem <- any
               guard $ pred elem
               pure elem

match :: forall seq elem rep pos m.
         Parsable seq elem rep pos => Monad m =>
         elem -> ParserT seq rep pos m elem
match = match_

string :: forall seq elem rep pos m.
          Parsable seq elem rep pos => Monad m =>
          seq -> ParserT seq rep pos m seq
string seq = if null <<< rep $ seq
             then consume *> pure seq
             else match_ seq

match_ :: forall seq rep pos m a.
        StripPrefix a rep => Update a pos => Monad m =>
        a -> ParserT seq rep pos m a
match_ a = do { rest, pos } <- gets $ un ParseState
              case stripPrefix a rest of
                Nothing   -> empty
                Just rest' -> do
                  put $ ParseState { rest: rest', pos: update a pos, consumed: true }
                  pure a

end :: forall seq elem rep pos m.
       Parsable seq elem rep pos => Monad m =>
       ParserT seq rep pos m Unit
end = do ended <- gets $ null <<< parseStateRest
         guard ended
         pure unit
