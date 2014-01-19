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
-- Module       : Control.Monad.PureDB.Base
-- Copyright    : (C) 2014 Dom De Re
-- License      : GPL 3 (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The PureStoreT functions that form the foundation
-- of working with the PureStore Monad.
-------------------------------------------------------------------
module Control.Monad.PureDB.Base (
    -- * Core Store Operations
        get
    ,   insert
    ,   insertNoKey
    ,   insertMany
    ,   insertKey
    ,   repsert
    ,   replace
    ,   delete
    ) where

import Control.Monad.PureDB.Class

import Prelude ( ($) )
import Control.Monad ( Monad )
import Control.Monad.Trans.Free ( liftF )
import Data.Function ( (.), flip, id )
import Data.Maybe ( Maybe )

-- |
-- Gets the value from the store corresponding
-- to the given key, returns `Nothing` if that value
-- doesnt exist in the store.
--
get :: (Monad m) => k -> PureStoreT k v m (Maybe v)
get = liftPureStoreF . flip Get id

-- |
-- Inserts the value into the store and then
-- returns the auto generated key
--
insert :: (Monad m) => v -> PureStoreT k v m k
insert = liftPureStoreF . flip Insert id

-- |
-- same as `insert` but it doesnt bother returning the
-- key
--
insertNoKey :: (Monad m) => v -> PureStoreT k v m ()
insertNoKey = liftPureStoreF . flip InsertNoKey ()

-- |
-- Inserts a batch of records and returns the list of
-- auto-generated ids
--
insertMany :: (Monad m) => [v] -> PureStoreT k v m [k]
insertMany = liftPureStoreF . flip InsertMany id

-- |
-- Inserts a record with a specified key...
--
insertKey :: (Monad m) => k -> v -> PureStoreT k v m ()
insertKey k v = liftPureStoreF $ InsertKey k v ()

-- |
-- If the record doesnt exist at the given key, it is inserted,
-- otherwise it replaces the record at the given key
--
repsert :: (Monad m) => k -> v -> PureStoreT k v m ()
repsert k v = liftPureStoreF $ Repsert k v ()

-- |
-- If no record exists in the store with the given id,
-- nothing happens, otherwise the existing value is replaced.
--
replace :: (Monad m) => k -> v -> PureStoreT k v m ()
replace k v = liftPureStoreF $ Replace k v ()

-- |
-- Deletes the record at the given key/id if it exists
-- otherwise nothing happens...
--
delete :: (Monad m) => k -> PureStoreT k v m ()
delete = liftPureStoreF . flip Delete ()

-- Helpers

liftPureStoreF :: (Monad m) => PureStoreF k v a -> PureStoreT k v m a
liftPureStoreF = PureStoreT . liftF
