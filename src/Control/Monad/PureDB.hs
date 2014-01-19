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
-- Module       : Control.Monad.PureDB
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
module Control.Monad.PureDB (
    -- * The Monad And Transformer
        PureStore
    ,   PureStoreT
    ) where

import Control.Monad.PureDB.Class

