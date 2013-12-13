{-
The MIT License (MIT)

Copyright (c) 2013 Dom De Re

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

-- | Experimenting with using my own non-yesod monad for the handlers.
--
-- Yesod is just too complicated when it comes to figuring out how to do the really simple
-- stuff that it just seems easier to get out of the yesod code and implement the simple
-- stuff myself instead of digging through the yesod code.
--
-- This will probably help with unit testing too
--
module Web.ZHandler where

import Prelude
    (   Show(..)
    ,   Eq(..)
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map
import qualified Data.Text as T
import Network.HTTP.Types.Header

data Method = Get | Post | Put | Delete deriving (Show, Eq)

data ZRequest = ZRequest
    {   getPrms         :: Map T.Text T.Text
    ,   getCookieVals   :: Map T.Text T.Text
    ,   reqHeaders      :: Map HeaderName BS.ByteString
    ,   getContent      :: LBS.ByteString
    } deriving (Show, Eq)

data ZHandler a = ZHandler deriving (Show, Eq)
