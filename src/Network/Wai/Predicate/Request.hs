-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Request
    ( Req
    , HasMethod  (..)
    , HasHeaders (..)
    , HasCookies (..)
    , HasQuery   (..)
    , HasPath    (..)
    , HasRequest (..)
    , HasVault   (..)

    , fromRequest
    , lookupHeader
    , lookupQuery
    , lookupCookie
    , lookupSegment
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!?))
import Data.Vault.Lazy (Vault)
import Data.Word
import Network.HTTP.Types
import Network.Wai (Request)
import Web.Cookie

import qualified Data.ByteString as B
import qualified Network.Wai     as Wai
import qualified Data.Vector     as Vec

class HasRequest a where
    getRequest :: a -> Wai.Request

class HasMethod a where
    method :: a -> Method

class HasHeaders a where
    headers :: a -> RequestHeaders

class HasCookies a where
    cookies :: a -> Cookies

class HasQuery a where
    queryItems :: a -> Query

class HasPath a where
    segments :: a -> Vector ByteString

class HasVault a where
    requestVault :: a -> Vault

data Req = Req
    { _request  :: Request
    , _cookies  :: Cookies
    , _segments :: Vector ByteString
    }

instance HasRequest Req where
    getRequest = _request

instance HasMethod Req where
    method = Wai.requestMethod . getRequest

instance HasMethod Wai.Request where
    method = Wai.requestMethod

instance HasHeaders Req where
    headers = Wai.requestHeaders . getRequest

instance HasHeaders Wai.Request where
    headers = Wai.requestHeaders

instance HasQuery Req where
    queryItems = Wai.queryString . getRequest

instance HasQuery Wai.Request where
    queryItems = Wai.queryString

instance HasCookies Req where
    cookies = _cookies

instance HasPath Req where
    segments = _segments

instance HasVault Req where
    requestVault = Wai.vault . _request

fromRequest :: Request -> Req
fromRequest rq =
    Req rq (concatMap parseCookies (getHeaders "Cookie" rq))
           (Vec.fromList . splitSegments . Wai.rawPathInfo $ rq)

lookupHeader :: HasHeaders r => HeaderName -> r -> [ByteString]
lookupHeader name = getHeaders name

lookupSegment :: HasPath r => Word -> r -> Maybe ByteString
lookupSegment i r = segments r !? fromIntegral i

lookupCookie :: HasCookies r => ByteString -> r -> [ByteString]
lookupCookie name = map snd . filter ((name ==) . fst) . cookies

lookupQuery :: HasQuery r => ByteString -> r -> [ByteString]
lookupQuery name = mapMaybe snd . filter ((name ==) . fst) . queryItems

getHeaders :: HasHeaders r => HeaderName -> r -> [ByteString]
getHeaders name = map snd . filter ((name ==) . fst) . headers

-----------------------------------------------------------------------------
-- Internal

splitSegments :: ByteString -> [ByteString]
splitSegments a
    | B.null a  = []
    | "/" == a  = []
    | otherwise = if B.head a == slash then go (B.tail a) else go a
  where
    go b =
        let (x, y) = B.breakByte slash b
        in urlDecode False x : if B.null y then [] else go (B.tail y)
    slash = 47
