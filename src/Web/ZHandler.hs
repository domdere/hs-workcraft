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
    -- | Auxiliary Types
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

newtype ZRESTLogReport = ZRESTLogReport ZRESTState

instance Show ZRESTLogReport where
    show (ZRESTLogReport s) = (intercalate "\n" . fmap showLogMsg . getLogMsgs) s
        where
            showLogMsg :: ZLogMessage -> String
            showLogMsg (Debug msg)  = "(Debug) " ++ show msg
            showLogMsg (Info msg)   = "(Info) " ++ show msg

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
-- >>> zErrorCode (ZServerError "")
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
-- >>> let debugLogLine = logMsg (Debug "Test Debug Log Line")
-- >>> let infoLogLine  = logMsg (Info "Test Info Log Line")
--
-- >>> (ZRESTLogReport . snd . testZHandler) debugLogLine
-- (Debug) "Test Debug Log Line"
--
-- >>> (ZRESTLogReport . snd . testZHandler) infoLogLine
-- (Info) "Test Info Log Line"
--
-- >>> (ZRESTLogReport . snd . testZHandler . sequence_) [debugLogLine, infoLogLine]
-- (Debug) "Test Debug Log Line"
-- (Info) "Test Info Log Line"
--
-- >>> (ZRESTLogReport . snd . testZHandler . sequence_) [infoLogLine, debugLogLine]
-- (Info) "Test Info Log Line"
-- (Debug) "Test Debug Log Line"
--
logMsg :: (Monad m) => ZLogMessage -> ZHandlerT m ()
logMsg msg = tell $ mempty { getLogMsgs = [msg] }

-- Helpers

liftRWE :: (RWE m a -> RWE m b) -> ZHandlerT m a -> ZHandlerT m b
liftRWE f = ZHandlerT . f . unwrap
