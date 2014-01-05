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

-- | code to generate and compare CSRF tokens

module Data.Csrf where

import Prelude
    (   Eq(..)
    ,   Show(..)
    ,   IO
    ,   ($)
    ,   fromIntegral
    )

import Control.Monad
import Control.Monad.Error
import Crypto.Classes ( constTimeEq )
import qualified Data.ByteString as BS
import Data.ByteString.Base64 ( encode )
import Data.Function ( on, (.) )
import Data.Functor
import Data.Int ( Int )
import Data.List ( (++), take )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import System.Random ( randoms )
import System.Random.Mersenne.Pure64 ( newPureMT )

newtype CSRFToken = CSRFToken { toByteString :: BS.ByteString } deriving (Show)

-- Instances

instance Eq CSRFToken where
    (==) = constTimeEq `on` toByteString

toText :: CSRFToken -> T.Text
toText = decodeUtf8 . toByteString

createRandomCsrf :: IO CSRFToken
createRandomCsrf = do
    prng <- newPureMT
    return $ (CSRFToken . encode . BS.pack . fmap fromIntegral) (take 24 (randoms prng :: [Int]))

readCsrfFromText :: (Monad m) => T.Text -> m CSRFToken
readCsrfFromText t =
    if T.length t == 32 then
        (return . CSRFToken . encodeUtf8) t
    else
        fail ("'" ++ T.unpack t ++ "' is not the right length for a CSRF token (" ++ show (T.length t) ++ " chars instead of 32 chars" )
