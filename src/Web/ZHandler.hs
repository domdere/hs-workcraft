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
module Web.ZHandler
    (   Method(..)
    ,   ZHeaders(..)
    ,   ZRequest(..)
    ,   ZError
    ,   ZHandler
    ,   ZLogMessage(..)
    ,   emptyResponse
--    ,   runZHandler
    ) where

import Prelude
    (   ($)
    ,   Show(..)
    ,   Eq(..)
    )

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ( on )
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Types.Header

-- Types

data Method = Get | Post | Put | Delete deriving (Show, Eq)

newtype ZHeaders = ZHeaders
    {   runHeaders :: M.Map HeaderName BS.ByteString
    } deriving (Show, Eq)

instance Monoid ZHeaders where
    mempty = ZHeaders $ M.fromList []

    mappend x y = ZHeaders ((M.union `on` runHeaders) x y)

data ZRequest = ZRequest
    {   getPrms         :: M.Map T.Text T.Text
    ,   getCookieVals   :: M.Map T.Text T.Text
    ,   reqHeaders      :: ZHeaders
    ,   getContent      :: LBS.ByteString
    } deriving (Show, Eq)

-- | The different error codes I have used so far
data ZError =
        NotFound
    |   NotAcceptable [T.Text]
    |   InvalidArgs [T.Text] deriving (Show, Eq)

data ZLogMessage =
        Debug T.Text
    |   Info T.Text deriving (Show, Eq)

data ZAccruedResponseState = ZAccruedResponseState
    {   getAllHeaders :: ZHeaders
    } deriving (Show, Eq)

instance Monoid ZAccruedResponseState where
    mempty = ZAccruedResponseState mempty

    x `mappend` y = ZAccruedResponseState
        ((mappend `on` getAllHeaders) x y)

data ZResponse a =
        Incomplete ZAccruedResponseState a
    |   Complete ZAccruedResponseState LBS.ByteString a deriving (Show, Eq)

instance Functor ZResponse where
    f `fmap` (Incomplete s x) = Incomplete s $ f x
    f `fmap` (Complete s b x) = Complete s b $ f x

instance Applicative ZResponse where
    pure = Incomplete mempty

    (Incomplete s f) <*> (Incomplete s' x) = Incomplete (s `mappend` s') $ f x
    (Incomplete s f) <*> (Complete s' b x) = Complete (s `mappend` s') b $ f x
    (Complete s b f) <*> x = Complete s b $ f (runResponse x)

instance Monad ZResponse where
    return = pure
    x >>= f = joinResponse $ f `fmap` x

newtype ZHandler a = ZHandler
    {   runZHandler :: ReaderT ZRequest (WriterT [ZLogMessage] ZResponse) a
    }

-- Functions

-- | Error codes for different error scenarios
--
-- >>> zErrorCode NotFound
-- 404
--
-- >>> zErrorCode (NotAcceptable [])
-- 406
--
-- >>> zErrorCode (InvalidArgs [])
-- 400
--
zErrorCode :: ZError -> Int
zErrorCode NotFound             = 404
zErrorCode (NotAcceptable _)    = 406
zErrorCode (InvalidArgs _)      = 400

-- | Creates the empty unfilled response
emptyResponse :: ZResponse ()
emptyResponse = Incomplete mempty ()

-- | extracts the value from the response
runResponse :: ZResponse a -> a
runResponse (Incomplete _ x) = x
runResponse (Complete _ _ x) = x

-- | flatten a response
--
joinResponse :: ZResponse (ZResponse a) -> ZResponse a
joinResponse (Incomplete s (Incomplete s' x))   = Incomplete (s `mappend` s') x
joinResponse (Incomplete s (Complete s' b x))   = Complete (s `mappend` s') b x
joinResponse (Complete s b (Incomplete _ x))    = Complete s b x
joinResponse (Complete s b (Complete _ _ x))    = Complete s b x
