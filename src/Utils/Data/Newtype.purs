module Utils.Data.Newtype where

import Data.Newtype (class Newtype, unwrap)
import Prelude ((<<<))

through :: forall t a b. Newtype t a => (a -> t) -> (a -> b) -> t -> b
through _ f = f <<< unwrap
