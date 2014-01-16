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
-- This module will be about translating to and from the HTTPResponse Monad
--
module Web.ZYesod where

import Prelude
    (   ($)
    ,   error
    ,   IO
    )

import qualified Data.ByteString.Lazy as BS
import Data.CaseInsensitive ( original )
import Data.Conduit.Lazy ( lazyConsume )
import Data.Either
import Data.Function ( (.), on )
import Data.Functor
import Data.Map ( fromList, toList )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import Data.Tuple (uncurry)
import Database.Persist.Sql ( SqlBackend )
import Control.Arrow
import Control.Monad ( return, sequence_ )
import Network.Wai
import Yesod
import Yesod.Core.Handler

import Control.Monad.HTTPResponse

class
    (   Yesod m
    ,   YesodPersist m
    ,   PersistUnique (YesodPersistBackend m (HandlerT m IO))
    ,   PersistQuery (YesodPersistBackend m (HandlerT m IO))
    ,   PersistMonadBackend (YesodPersistBackend m (HandlerT m IO)) ~ SqlBackend
    ) => ZYesod m

convertHTTPResponseIO :: (ZYesod m) => HTTPResponseT IO a -> HandlerT m IO a
convertHTTPResponseIO z =
    let
        prepareHeaders = fmap ((decodeUtf8 . original) *** decodeUtf8) . toList . runHeaders . getAllHeaders
        prepareCookies = toList . cookieMap . getSetCookie
    in do
        yesodReq <- getRequest
        waiReq <- waiRequest
        body <- (lift . lazyConsume . requestBody) waiReq
        let headers = (HTTPHeaders . fromList . requestHeaders) waiReq
        let getParams = (fromList . reqGetParams) yesodReq
        let cookieParams = (CookieMap . fromList . reqCookies) yesodReq
        let reqBody = BS.fromChunks body
        let req = HTTPRequest getParams cookieParams headers reqBody
        (eResult, s) <- liftIO $ runHTTPResponseT req z
        -- make sure to log everything..
        sequence_ $ logZMsg <$> getLogMsgs s
        case eResult of
            -- now to determine the response, first check if there was an error
            -- if there was, we dont need to set the headers, Yesod can set them
            -- appropriately depending on the users request headers...
            Left HTTPNotFound                  -> notFound
            Left (HTTPServerError msg)         -> error (T.unpack msg)
            Left (HTTPPermissionDenied msg)    -> permissionDenied msg
            Left HTTPBadMethod                 -> badMethod
            Left (HTTPInvalidArgs msgs)        -> invalidArgs msgs
            Right val                       -> do
                -- set the headers and session values and return the result
                sequence_ $ uncurry addHeader <$> prepareHeaders s
                sequence_ $ uncurry setSession <$> prepareCookies s
                return val

logZMsg :: HTTPLogMessage -> HandlerT m IO ()
logZMsg (Info msg)  = $(logInfo) msg
logZMsg (Debug msg) = $(logDebug) msg
