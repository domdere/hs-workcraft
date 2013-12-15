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
    ,   ZCookie(..)
    ,   ZHeaders(..)
    ,   ZRequest(..)
    ,   ZError
    ,   ZHandler
    ,   ZHandlerT
    ,   ZLogMessage(..)
    ,   runZHandlerT
    ) where

import Prelude
    (   ($)
    ,   Show(..)
    ,   Eq(..)
    ,   id
    )

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Function ( (.), on )
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

    mappend x y = ZHeaders $ (M.union `on` runHeaders) x y

newtype ZCookie = ZCookie
    {   cookieMap :: M.Map T.Text T.Text
    } deriving (Show, Eq)

instance Monoid ZCookie where
    mempty = ZCookie $ M.fromList []

    mappend x y = ZCookie $ (M.union `on` cookieMap) x y

data ZRequest = ZRequest
    {   getPrms         :: M.Map T.Text T.Text
    ,   getCookieVals   :: ZCookie
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

-- | Represents the accrued headers and cookie key-value settings
-- from a REST computation
data ZRESTState = ZRESTState
    {   getAllHeaders   :: ZHeaders
    ,   getSetCookie    :: ZCookie
    } deriving (Show, Eq)

instance Monoid ZRESTState where
    mempty = ZRESTState mempty mempty

    x `mappend` y = ZRESTState
        ((mappend `on` getAllHeaders) x y)
        ((mappend `on` getSetCookie) x y)

-- | we want error reporting, logging, accumulation of headers and cookie values,
-- and its going to read in the original resquest that spurred off the original REST
-- computation.
newtype ZHandlerT m a = ZHandlerT
    {   unwrap :: WriterT [ZLogMessage] (ErrorT ZError m) a
    }

type ZHandler a = ZHandlerT Identity a

-- Functions

runZHandlerTStack :: (Monad m) => ZHandlerT m a -> m (Either ZError (a, [ZLogMessage]))
runZHandlerTStack = runErrorT . runWriterT . unwrap

rearrange :: a -> a
rearrange = id

-- | I want to extract an m ((Either ZError a, [ZLogLine], ZRESTState))
runZHandlerT :: (Monad m) => ZHandlerT m a -> m (Either ZError (a, [ZLogMessage]))
runZHandlerT = liftM rearrange . runZHandlerTStack

-- | 
runZHandler :: ZHandler a -> Either ZError (a, [ZLogMessage])
runZHandler = runIdentity . runZHandlerT

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

