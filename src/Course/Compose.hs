{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) p (Compose fga) =
    Compose ((p <$>) <$> fga) -- still not very clear about '(p <$>)' part...

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure =
    -- Compose (pure (pure a))
    Compose . pure . pure -- observer how the (.) works with type

-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose fgab) (Compose fga) =
--    Compose (((<*>) <$> fgab) <*> fga)
--    Compose ((<*>) <$> fgab <*> fga) -- remove brackets
    Compose (lift2 (<*>) fgab fga) -- check the implementation of list2

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
