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
    -- | Auxiliary Types
    (   Method(..)
    ,   ZCookie(..)
    ,   ZHeaders(..)
    ,   ZRequest(..)
    ,   ZError(..)
    ,   ZLogMessage(..)
    ,   ZRESTReport(..)
    ,   ZRESTState(..)

    -- | The Monad
    ,   ZHandler
    ,   ZHandlerT

    -- | Writer Helpers
    ,   addHeaders
    ,   logMsg
    ,   putSessionValues

    -- | Unwrapping the monad
    ,   runZHandler
    ,   runZHandlerT
    ) where

import Prelude
    (   ($)
    ,   Show(..)
    ,   Eq(..)
    ,   flip
    ,   id
    )

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
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

-- $setup
--
-- >>> import Test.QuickCheck
-- >>> import Data.Tuple
-- >>> let testZHandler = runZHandler emptyRequest

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
        ZNotFound
    |   ZServerError T.Text
    |   ZBadMethod
    |   ZPermissionDenied T.Text
    |   ZInvalidArgs [T.Text] deriving (Show, Eq)

instance Error ZError where
    noMsg = ZServerError ""

    strMsg s = ZServerError (T.pack s)

data ZLogMessage =
        Debug T.Text
    |   Info T.Text deriving (Show, Eq)

-- | Represents the accrued headers and cookie key-value settings
-- from a REST computation
data ZRESTState = ZRESTState
    {   getAllHeaders   :: ZHeaders
    ,   getSetCookie    :: ZCookie
    ,   getLogMsgs      :: [ZLogMessage]
    } deriving (Show, Eq)

instance Monoid ZRESTState where
    mempty = ZRESTState mempty mempty mempty

    x `mappend` y = ZRESTState
        ((mappend `on` getAllHeaders) x y)
        ((mappend `on` getSetCookie) x y)
        ((mappend `on` getLogMsgs) x y)

newtype ZRESTReport = ZRESTReport ZRESTState

instance Show ZRESTReport where
    show (ZRESTReport s) = intercalate "\n" $ join
        [   ["Headers:"]
        ,   showMap headers
        ,   ["Session Values:"]
        ,   showMap cookie
        ]
        where
            showMap = fmap (\(k, v) -> show k ++ ": " ++ show v)
            headers = (M.toList . runHeaders . getAllHeaders) s
            cookie  = (M.toList . cookieMap . getSetCookie) s

type RWE m a = ReaderT ZRequest (ErrorT ZError (WriterT ZRESTState m)) a

-- | we want error reporting, logging, accumulation of headers and cookie values,
-- and its going to read in the original request that spurred off the original REST
-- computation.
newtype ZHandlerT m a = ZHandlerT
    {   unwrap :: RWE m a
    }

instance (Monad m) => Monad (ZHandlerT m) where
    return = ZHandlerT . return

    (ZHandlerT ma) >>= f = ZHandlerT $ ma >>= (unwrap . f)

instance MonadTrans ZHandlerT where
    lift = ZHandlerT . lift . lift . lift

instance (Monad m) => MonadWriter ZRESTState (ZHandlerT m) where
    writer = ZHandlerT . writer

    listen = liftRWE listen

    pass = liftRWE pass

type ZHandler a = ZHandlerT Identity a

-- Functions

runZHandlerT :: (Monad m) => ZRequest -> ZHandlerT m a -> m (Either ZError a, ZRESTState)
runZHandlerT req = runWriterT . runErrorT . flip runReaderT req . unwrap

runZHandler :: ZRequest -> ZHandler a -> (Either ZError a, ZRESTState)
runZHandler = (runIdentity .) . runZHandlerT

-- | Error codes for different error scenarios
--
-- >>> zErrorCode ZNotFound
-- 404
--
-- >>> zErrorCode ZBadMethod
-- 405
--
-- >>> zErrorCode (ZPermissionDenied "")
-- 403
--
-- >>> zErrorCode (ZInvalidArgs [])
-- 400
--
-- >>> zErrorCode (ZServerError [])
-- 500
--
zErrorCode :: ZError -> Int
zErrorCode ZNotFound                = 404
zErrorCode (ZServerError _)         = 500
zErrorCode ZBadMethod               = 405
zErrorCode (ZPermissionDenied _)    = 403
zErrorCode (ZInvalidArgs _)         = 400

emptyRequest :: ZRequest
emptyRequest = ZRequest (M.fromList []) (ZCookie $ M.fromList []) (ZHeaders $ M.fromList []) ""

-- | An alias for "," that makes the Header assignment like more of the form "Header : Value"
-- in most plaintext serialisations of Headers
--
-- >>> "Content-Type" .: "application/json"
-- ("Content-Type","application/json")
--
(.:) :: a -> b -> (a, b)
(.:) = (,)

-- | Sets a header
--
-- >>> ZRESTReport $ snd $ testZHandler (addHeaders ["Content-Type" .: "application/xml", "Some-Other-Header" .: "And its value"])
-- Headers:
-- "Content-Type": "application/xml"
-- "Some-Other-Header": "And its value"
-- Session Values:
--
addHeaders :: (Monad m) => [(HeaderName, BS.ByteString)] -> ZHandlerT m ()
addHeaders kvs = tell $ mempty { getAllHeaders = ZHeaders $ M.fromList kvs }

-- | Writes session values
--
-- >>> ZRESTReport $ snd $ testZHandler (putSessionValues ["_id" .: "blah234356d", "somethingelse" .: "blach"])
-- Headers:
-- Session Values:
-- "_id": "blah234356d"
-- "somethingelse": "blach"
--
putSessionValues :: (Monad m) => [(T.Text, T.Text)] -> ZHandlerT m ()
putSessionValues kvs = tell $ mempty { getSetCookie = ZCookie $ M.fromList kvs }

-- | For writing log messages...
--
logMsg :: (Monad m) => ZLogMessage -> ZHandlerT m ()
logMsg msg = tell $ mempty { getLogMsgs = [msg] }

-- Helpers

liftRWE :: (RWE m a -> RWE m b) -> ZHandlerT m a -> ZHandlerT m b
liftRWE f = ZHandlerT . f . unwrap
