-- | Combinators for creating parsers.
-- |
-- | ### Notes
-- |
-- | A few of the known combinators from Parsec are missing in this module. That
-- | is because they have already been defined in other libraries.
-- |
-- | ```purescript
-- | Text.Parsec.many  = Data.(Array|List).many
-- | Text.Parsec.many1 = Data.(Array|List).some
-- | Text.Parsec.(<|>) = Control.Alt.alt (<|>)
-- | ```
-- |
-- | Because Strings are not Char Arrays in PureScript `many` and `some` on Char Parsers need to
-- | be used in conjunction with `Data.String.CodeUnits.fromCharArray` to achieve "Parsec-like" results.
-- |
-- | ```purescript
-- | Text.Parsec.many  (char 'x') <=> fromCharArray <$> Data.Array.many (char 'x')
-- | ```
-- |
-- | Note that `Data.(Array|List).(many|some)` are not stack safe. If you need to parse
-- | large numbers of items then consider using `Data.List.(manyRec|someRec)` instead.

module Sequence.Parsing.Parser.Combinators where

import Prelude

import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.State (StateT(..), runStateT)
import Control.Plus (empty, (<|>))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:), many, some, singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un)
import Data.Tuple (Tuple(..))
import Sequence.Parsing.Parser (ParserT(..), fail)
import Sequence.Parsing.Parser.Data (ParseError(..), ParseState(..))
import Utils.Data.Newtype (through)

-- | Provide an error message in the case of failure.
withErrorMessage :: forall seq rep pos m a. Monad m =>
                    ParserT seq rep pos m a -> String -> ParserT seq rep pos m a
withErrorMessage p msg = p <|> fail msg

infixl 3 withErrorMessage as <?>

-- | Flipped `(<?>)`.
asErrorMessage :: forall seq rep pos m a. Monad m => String -> ParserT seq rep pos m a -> ParserT seq rep pos m a
asErrorMessage = flip (<?>)

infixl 3 asErrorMessage as <??>

-- | Wrap a parser with opening and closing markers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | parens = between (string "(") (string ")")
-- | ```
between :: forall seq rep pos m a open close.
           Monad m =>
           ParserT seq rep pos m open -> ParserT seq rep pos m close -> ParserT seq rep pos m a ->
           ParserT seq rep pos m a
between open close p = open *> p <* close

-- | Provide a default result in the case where a parser fails without consuming input.
option :: forall seq rep pos m a. Monad m => a -> ParserT seq rep pos m a -> ParserT seq rep pos m a
option a p = p <|> pure a

-- | Optionally parse something, failing quietly.
optional :: forall seq rep pos m a. Monad m => ParserT seq rep pos m a -> ParserT seq rep pos m Unit
optional p = void p <|> pure unit

-- | pure `Nothing` in the case where a parser fails without consuming input.
optionMaybe :: forall seq rep pos m a. Monad m => ParserT seq rep pos m a -> ParserT seq rep pos m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | In case of failure, reset the sequence to the unconsumed state.
try :: forall seq rep pos m a. Monad m =>
       ParserT seq rep pos  m a -> ParserT seq rep pos m a
try p = ParserT <<< ExceptT <<< StateT $ \s  -> do
  result@(Tuple e s') <- runStateT (runExceptT (un ParserT p)) s
  case e of
    Left _ -> pure $ Tuple e parseState where
      parseState = over ParseState _ { consumed = through ParseState _.consumed s } s'
    _      -> pure result

-- | Like `try`, but will reannotate the error location to the `try` point.
tryRethrow :: forall seq rep pos m a. Monad m =>
              ParserT seq rep pos m a -> ParserT seq rep pos m a
tryRethrow p = ParserT <<< ExceptT <<< StateT $ \s -> do
  result@(Tuple e s') <- runStateT (runExceptT (un ParserT p)) s
  case e of
    Left pe -> pure $ Tuple (Left parseError) parseState where
      { pos, consumed } = un ParseState s
      parseError = over ParseError _ { pos = pos } pe
      parseState = over ParseState _ { consumed = consumed } s'
    _ -> pure result

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead :: forall seq rep pos m a. Monad m => ParserT seq rep pos m a -> ParserT seq rep pos m a
lookAhead p = (ParserT <<< ExceptT <<< StateT) \s -> do
  Tuple e _ <- runStateT (runExceptT (un ParserT p)) s
  pure (Tuple e s)

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall seq rep pos m a sep. Monad m =>
         ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall seq rep pos m a sep. Monad m =>
          ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
sepBy1 p sep = do
  a <- p
  as <- many $ sep *> p
  pure (a : as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall seq rep pos m a sep. Monad m =>
            ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
sepEndBy p sep = sepEndBy1 p sep <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall seq rep pos m a sep. Monad m =>
             ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
sepEndBy1 p sep = do
  a <- p
  let many = do _ <- sep
                as <- sepEndBy p sep
                pure (a : as)
  many <|> pure (singleton a)

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall seq rep pos m a sep. Monad m =>
          ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
endBy1 p sep = some $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall seq rep pos m a sep. Monad m =>
         ParserT seq rep pos m a -> ParserT seq rep pos m sep -> ParserT seq rep pos m (List a)
endBy p sep = many $ p <* sep

-- | Parse phrases delimited by a right-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" *> add) 0
-- | ```
chainr :: forall seq rep pos m a. Monad m =>
                            ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> a -> ParserT seq rep pos m a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator.
chainl :: forall seq rep pos m a. Monad m =>
          ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> a -> ParserT seq rep pos m a
chainl p f a = chainl1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator, requiring at least one match.
chainl1 :: forall seq rep pos m a. Monad m =>
           ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> ParserT seq rep pos m a
chainl1 p f = do a <- p
                 chainl1' p f a

chainl1' :: forall seq rep pos m a. Monad m =>
            ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> a -> ParserT seq rep pos m a
chainl1' p f a = do f' <- f
                    a' <- p
                    chainl1' p f (f' a a')
                 <|> pure a

-- | Parse phrases delimited by a right-associative operator, requiring at least one match.
chainr1 :: forall seq rep pos m a. Monad m =>
           ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> ParserT seq rep pos m a
chainr1 p f = do a <- p
                 chainr1' p f a

chainr1' :: forall seq rep pos m a. Monad m =>
            ParserT seq rep pos m a -> ParserT seq rep pos m (a -> a -> a) -> a -> ParserT seq rep pos m a
chainr1' p f a = do f' <- f
                    a' <- chainr1 p f
                    pure $ f' a a'
                 <|> pure a

-- | Parse one of a set of alternatives.
choice :: forall f seq rep pos m a. Foldable f => Monad m =>
          f (ParserT seq rep pos m a) -> ParserT seq rep pos m a
choice = foldl (<|>) empty

-- | Skip many instances of a phrase.
skipMany :: forall seq rep pos m a. Monad m =>
            ParserT seq rep pos m a -> ParserT seq rep pos m Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall seq rep pos m a. Monad m =>
             ParserT seq rep pos m a -> ParserT seq rep pos m Unit
skipMany1 p = do
  x <- p
  xs <- skipMany p
  pure unit

-- | Fail if the specified parser matches.
notFollowedBy :: forall seq rep pos m a. Monad m =>
                 ParserT seq rep pos m a -> ParserT seq rep pos m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse several phrases until the specified terminator matches.
manyTill :: forall seq rep pos m a end. Monad m =>
            ParserT seq rep pos m a -> ParserT seq rep pos m end -> ParserT seq rep pos m (List a)
manyTill p end = scan
  where
    scan = (end $> Nil)
           <|> do x <- p
                  xs <- scan
                  pure (x:xs)

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
many1Till :: forall seq rep pos m a end. Monad m =>
             ParserT seq rep pos m a -> ParserT seq rep pos m end -> ParserT seq rep pos m (List a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (x:xs)
