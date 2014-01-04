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
-- This module will be about translating to and from the ZHandler Monad
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

import Web.ZHandler

class
    (   Yesod m
    ,   YesodPersist m
    ,   PersistUnique (YesodPersistBackend m (HandlerT m IO))
    ,   PersistQuery (YesodPersistBackend m (HandlerT m IO))
    ,   PersistMonadBackend (YesodPersistBackend m (HandlerT m IO)) ~ SqlBackend
    ) => ZYesod m

-- | WIP: This should pull the info of interest into a Request
-- and then run a ZHandler Monad with it, but i havent finished
-- writing the monad, i just want to make sure I know how to get the info i want
-- with Yesod...
convertZHandlerIO :: (ZYesod m) => ZHandlerT IO a -> HandlerT m IO a
convertZHandlerIO z =
    let
        prepareHeaders = fmap ((decodeUtf8 . original) *** decodeUtf8) . toList . runHeaders . getAllHeaders
        prepareCookies = toList . cookieMap . getSetCookie
    in do
        yesodReq <- getRequest
        waiReq <- waiRequest
        body <- (lift . lazyConsume . requestBody) waiReq
        let headers = (ZHeaders . fromList . requestHeaders) waiReq
        let getParams = (fromList . reqGetParams) yesodReq
        let cookieParams = (ZCookie . fromList . reqCookies) yesodReq
        let reqBody = BS.fromChunks body
        let req = ZRequest getParams cookieParams headers reqBody
        (eResult, s) <- liftIO $ runZHandlerT req z
        -- make sure to log everything..
        sequence_ $ logZMsg <$> getLogMsgs s
        case eResult of
            -- now to determine the response, first check if there was an error
            -- if there was, we dont need to set the headers, Yesod can set them
            -- appropriately depending on the users request headers...
            Left ZNotFound                  -> notFound
            Left (ZServerError msg)         -> error (T.unpack msg)
            Left (ZPermissionDenied msg)    -> permissionDenied msg
            Left ZBadMethod                 -> badMethod
            Left (ZInvalidArgs msgs)        -> invalidArgs msgs
            Right val                       -> do
                -- set the headers and session values and return the result
                sequence_ $ uncurry addHeader <$> prepareHeaders s
                sequence_ $ uncurry setSession <$> prepareCookies s
                return val

logZMsg :: ZLogMessage -> HandlerT m IO ()
logZMsg (Info msg)  = $(logInfo) msg
logZMsg (Debug msg) = $(logDebug) msg
