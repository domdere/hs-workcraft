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

-- |
-- Experimenting with using my own pure non-yesod monad for the handlers.
--
-- Yesod is just too complicated when it comes to figuring out how to do the really simple
-- stuff that it just seems easier to get out of the yesod code and implement the simple
-- stuff myself instead of digging through the yesod code.
--
-- This will probably help with unit testing too
--
-- NOTE: An alternative construction of a Pure Monad to help with Proofs and Testing
-- could be to use a Free Monad, I'll try this next time...
--
module Control.Monad.HTTPResponse
    (   Method(..)
    -- * Auxiliary Types
    ,   CookieMap(..)
    ,   HTTPHeaders(..)
    ,   HTTPRequest(..)
    ,   HTTPError(..)
    ,   HTTPLogMessage(..)
    ,   HTTPResponseState(..)

    -- * The Monad
    ,   HTTPResponse
    ,   HTTPResponseT

    -- * Writer Helpers
    ,   addHeaders
    ,   logMsg
    ,   putSessionValues

    -- * Unwrapping the monad
    ,   runHTTPResponse
    ,   runHTTPResponseT

    -- * Unit Testing the Pure HTTPResponse Monad
    ,   HTTPReport(..)
    ,   HTTPLogReport(..)
    ) where

import Prelude
    (   String
    ,   Show(..)
    ,   Eq(..)
    ,   ($)
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
-- >>> let testHTTPResponse = runHTTPResponse emptyRequest

-- Types

data Method = Get | Post | Put | Delete deriving (Show, Eq)

newtype HTTPHeaders = HTTPHeaders
    {   runHeaders :: M.Map HeaderName BS.ByteString
    } deriving (Show, Eq)

instance Monoid HTTPHeaders where
    mempty = HTTPHeaders $ M.fromList []

    mappend x y = HTTPHeaders $ (M.union `on` runHeaders) x y

newtype CookieMap = CookieMap
    {   cookieMap :: M.Map T.Text T.Text
    } deriving (Show, Eq)

instance Monoid CookieMap where
    mempty = CookieMap $ M.fromList []

    mappend x y = CookieMap $ (M.union `on` cookieMap) x y

data HTTPRequest = HTTPRequest
    {   getPrms         :: M.Map T.Text T.Text
    ,   getCookieVals   :: CookieMap
    ,   reqHeaders      :: HTTPHeaders
    ,   getContent      :: LBS.ByteString
    } deriving (Show, Eq)

-- | The different error codes I have used so far
data HTTPError =
        HTTPNotFound
    |   HTTPServerError T.Text
    |   HTTPBadMethod
    |   HTTPPermissionDenied T.Text
    |   HTTPInvalidArgs [T.Text] deriving (Show, Eq)

instance Error HTTPError where
    noMsg = HTTPServerError ""

    strMsg s = HTTPServerError (T.pack s)

data HTTPLogMessage =
        Debug T.Text
    |   Info T.Text deriving (Show, Eq)

-- | Represents the accrued headers and cookie key-value settings
-- from a REST computation
data HTTPResponseState = HTTPResponseState
    {   getAllHeaders   :: HTTPHeaders
    ,   getSetCookie    :: CookieMap
    ,   getLogMsgs      :: [HTTPLogMessage]
    } deriving (Show, Eq)

instance Monoid HTTPResponseState where
    mempty = HTTPResponseState mempty mempty mempty

    x `mappend` y = HTTPResponseState
        ((mappend `on` getAllHeaders) x y)
        ((mappend `on` getSetCookie) x y)
        ((mappend `on` getLogMsgs) x y)

-- | Wrap a HTTPResponseState with this in a doctest and it will result in the header and cookie values being displayed.
newtype HTTPReport = HTTPReport HTTPResponseState

instance Show HTTPReport where
    show (HTTPReport s) = intercalate "\n" $ join
        [   ["Headers:"]
        ,   showMap headers
        ,   ["Session Values:"]
        ,   showMap cookie
        ]
        where
            showMap = fmap (\(k, v) -> show k ++ ": " ++ show v)
            headers = (M.toList . runHeaders . getAllHeaders) s
            cookie  = (M.toList . cookieMap . getSetCookie) s

-- | Wrap a HTTPResponseState with this in a doctest and it will result in the log lines associated with the state being displayed.
newtype HTTPLogReport = HTTPLogReport HTTPResponseState

instance Show HTTPLogReport where
    show (HTTPLogReport s) = (intercalate "\n" . fmap showLogMsg . getLogMsgs) s
        where
            showLogMsg :: HTTPLogMessage -> String
            showLogMsg (Debug msg)  = "(Debug) " ++ show msg
            showLogMsg (Info msg)   = "(Info) " ++ show msg

type RWE m a = ReaderT HTTPRequest (ErrorT HTTPError (WriterT HTTPResponseState m)) a

-- | we want error reporting, logging, accumulation of headers and cookie values,
-- and its going to read in the original request that spurred off the original REST
-- computation.
newtype HTTPResponseT m a = HTTPResponseT
    {   unwrap :: RWE m a
    }

instance (Monad m) => Monad (HTTPResponseT m) where
    return = HTTPResponseT . return

    (HTTPResponseT ma) >>= f = HTTPResponseT $ ma >>= (unwrap . f)

instance MonadTrans HTTPResponseT where
    lift = HTTPResponseT . lift . lift . lift

instance (Monad m) => MonadWriter HTTPResponseState (HTTPResponseT m) where
    writer = HTTPResponseT . writer

    listen = liftRWE listen

    pass = liftRWE pass

type HTTPResponse a = HTTPResponseT Identity a

-- Functions

runHTTPResponseT :: (Monad m) => HTTPRequest -> HTTPResponseT m a -> m (Either HTTPError a, HTTPResponseState)
runHTTPResponseT req = runWriterT . runErrorT . flip runReaderT req . unwrap

runHTTPResponse :: HTTPRequest -> HTTPResponse a -> (Either HTTPError a, HTTPResponseState)
runHTTPResponse = (runIdentity .) . runHTTPResponseT

-- | Error codes for different error scenarios
--
-- >>> zErrorCode HTTPNotFound
-- 404
--
-- >>> zErrorCode HTTPBadMethod
-- 405
--
-- >>> zErrorCode (HTTPPermissionDenied "")
-- 403
--
-- >>> zErrorCode (HTTPInvalidArgs [])
-- 400
--
-- >>> zErrorCode (HTTPServerError "")
-- 500
--
zErrorCode :: HTTPError -> Int
zErrorCode HTTPNotFound                = 404
zErrorCode (HTTPServerError _)         = 500
zErrorCode HTTPBadMethod               = 405
zErrorCode (HTTPPermissionDenied _)    = 403
zErrorCode (HTTPInvalidArgs _)         = 400

emptyRequest :: HTTPRequest
emptyRequest = HTTPRequest (M.fromList []) (CookieMap $ M.fromList []) (HTTPHeaders $ M.fromList []) ""

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
-- >>> HTTPReport $ snd $ testHTTPResponse (addHeaders ["Content-Type" .: "application/xml", "Some-Other-Header" .: "And its value"])
-- Headers:
-- "Content-Type": "application/xml"
-- "Some-Other-Header": "And its value"
-- Session Values:
--
addHeaders :: (Monad m) => [(HeaderName, BS.ByteString)] -> HTTPResponseT m ()
addHeaders kvs = tell $ mempty { getAllHeaders = HTTPHeaders $ M.fromList kvs }

-- | Writes session values
--
-- >>> HTTPReport $ snd $ testHTTPResponse (putSessionValues ["_id" .: "blah234356d", "somethingelse" .: "blach"])
-- Headers:
-- Session Values:
-- "_id": "blah234356d"
-- "somethingelse": "blach"
--
putSessionValues :: (Monad m) => [(T.Text, T.Text)] -> HTTPResponseT m ()
putSessionValues kvs = tell $ mempty { getSetCookie = CookieMap $ M.fromList kvs }

-- | For writing log messages...
--
-- >>> let debugLogLine = logMsg (Debug "Test Debug Log Line")
-- >>> let infoLogLine  = logMsg (Info "Test Info Log Line")
--
-- >>> (HTTPLogReport . snd . testHTTPResponse) debugLogLine
-- (Debug) "Test Debug Log Line"
--
-- >>> (HTTPLogReport . snd . testHTTPResponse) infoLogLine
-- (Info) "Test Info Log Line"
--
-- >>> (HTTPLogReport . snd . testHTTPResponse . sequence_) [debugLogLine, infoLogLine]
-- (Debug) "Test Debug Log Line"
-- (Info) "Test Info Log Line"
--
-- >>> (HTTPLogReport . snd . testHTTPResponse . sequence_) [infoLogLine, debugLogLine]
-- (Info) "Test Info Log Line"
-- (Debug) "Test Debug Log Line"
--
logMsg :: (Monad m) => HTTPLogMessage -> HTTPResponseT m ()
logMsg msg = tell $ mempty { getLogMsgs = [msg] }

-- Helpers

liftRWE :: (RWE m a -> RWE m b) -> HTTPResponseT m a -> HTTPResponseT m b
liftRWE f = HTTPResponseT . f . unwrap
