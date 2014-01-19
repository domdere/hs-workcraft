{-
Copyright (c) 2014 Dom De Re

This file is part of Workcraft.

Workcraft is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Workcraft is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Workcraft.  If not, see <http://www.gnu.org/licenses/>.
-}

-------------------------------------------------------------------
-- |
-- Module       : Control.Monad.PureDB.Class
-- Copyright    : (C) 2014 Dom De Re
-- License      : GPL 3 (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A Pure Monad for managing DB operations and making them a little
-- more unit testable, with HTTPResponse I used a stack of
-- other standard transformers (ReaderT, WriterT, ErrorT etc..)
-- to create a Pure Monad for HTTP responses...
--
-- This time I want to give Free Monads a try (just to learn
-- about Free Monads), after having read this article:
--
-- <http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html>
-------------------------------------------------------------------
module Control.Monad.PureDB.Class (
    -- * The Monad And Transformer
        PureStore
    ,   PureStoreT'
    ,   PureStoreT(..)
    -- * The Underlying Functor
    ,   PureStoreF(..)
    ) where

import Prelude ( Show )
import Control.Monad ( Monad(..), liftM )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Trans.Free
import Data.Enumerator
import Data.Function
import Data.Functor
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Maybe ( Maybe )

-- |
--  The Functor that will form the basis of the Free Monad...
--  it "models" the `PersistStore` class of `Database.Persist`
--
--  Cons:
--
--      -   Each Monad instance only handles DB ops of of a certain record (and key) type...
--
--      -   Therefore record types for an app must have a hierarchy that allows
--          for all the DB operations to be grouped up per type and sorted by type (with an
--          ordering thats constant over the app) without causing them to fail... (that seems like
--          a big ask, but maybe that requirement just makes the design simpler).
--
--  Pros:
--
--      -   The monad's single record type enables a possible homomorphism
--          from (PureStoreT k v m a) to (StateT (Map k v, Stream k) m a) allowing easier/simpler equational
--          reasoning and unit testing.
--
data PureStoreF k v a =
        Get k (Maybe v -> a)
    |   Insert v (k -> a)
    |   InsertNoKey v a
    |   InsertMany [v] ([k] -> a)
    |   InsertKey k v a
    |   Repsert k v a
    |   Replace k v a
    |   Delete k a

instance Functor (PureStoreF k v) where
    fmap f (Get k g)            = Get k $ f . g
    fmap f (Insert v g)         = Insert v $ f . g
    fmap f (InsertNoKey v x)    = InsertNoKey v $ f x
    fmap f (InsertMany vs g)    = InsertMany vs $ f . g
    fmap f (InsertKey k v x)    = InsertKey k v $ f x
    fmap f (Repsert k v x)      = Repsert k v $ f x
    fmap f (Replace k v x)      = Replace k v $ f x
    fmap f (Delete k x)         = Delete k $ f x

-- | Wraps it up in a Free Monad Transformer...
--
type PureStoreT' k v m a = FreeT (PureStoreF k v) m a

newtype PureStoreT k v m a = PureStoreT { unwrap :: PureStoreT' k v m a }

type PureStore k v a = PureStoreT k v Identity a

-- `PureStoreT` instances

instance (Monad m) => Functor (PureStoreT k v m) where
    fmap = liftPureStoreT' . fmap

instance (Monad m) => Monad (PureStoreT k v m) where
    return = PureStoreT . return

    ma >>= f = PureStoreT $ unwrap ma >>= (unwrap . f)

instance MonadTrans (PureStoreT k v) where
    lift = PureStoreT . lift

-- Helpers

-- | convenient for lifting FreeT that we want to keep up to PureStoreT
liftPureStoreT' :: (PureStoreT' k v m a -> PureStoreT' k v m b) -> PureStoreT k v m a -> PureStoreT k v m b
liftPureStoreT' f = PureStoreT . f . unwrap
