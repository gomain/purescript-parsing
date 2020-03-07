module Sequence.Parsing.Parser.Primitives (
  any,
  when,
  match,
  string,
  end ) where

import Prelude

import Control.Monad.State (gets, put)
import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Sequence.Parsing.Parser (ParserT, consume, fail)
import Sequence.Parsing.Parser.Class (class Drop, class Parsable, class Update, drop, null, rep, uncons, update)
import Sequence.Parsing.Parser.Data (ParseState(..), parseStateRest)

any :: forall seq elem rep pos m.
       Parsable seq elem rep pos => Monad m =>
       ParserT seq rep pos m elem
any = do
  { rest, pos } <- gets $ un ParseState
  case uncons rest of
    Nothing -> fail "Unexpected end of sequence"
    Just { head, tail } -> do
      put $ ParseState { rest: tail, pos: update head pos, consumed: true }
      pure head

when :: forall seq elem rep pos m.
        Parsable seq elem rep pos => Monad m =>
        (elem -> Boolean) -> ParserT seq rep pos m elem
when pred = do elem <- any
               guard $ pred elem
               pure elem

match :: forall seq elem rep pos m.
         Parsable seq elem rep pos => Monad m =>
         elem -> ParserT seq rep pos m elem
match = take

string :: forall seq elem rep pos m.
          Parsable seq elem rep pos => Monad m =>
          seq -> ParserT seq rep pos m seq
string seq = if null <<< rep $ seq
             then consume *> pure seq
             else take seq

take :: forall seq rep pos m a.
        Drop a rep => Update a pos => Monad m =>
        a -> ParserT seq rep pos m a
take a = do { rest, pos } <- gets $ un ParseState
            case drop a rest of
              Nothing   -> fail "Parse failed"
              Just rest' -> do
                put $ ParseState { rest: rest', pos: update a pos, consumed: true }
                pure a

end :: forall seq elem rep pos m.
       Parsable seq elem rep pos => Monad m =>
       ParserT seq rep pos m Unit
end = do ended <- gets $ null <<< parseStateRest
         guard ended
         pure unit
