-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate
    ( module Data.Predicate
    , request

    , def
    , opt

    , query
    , hasQuery

    , header
    , hasHeader

    , segment
    , hasSegment

    , cookie
    , hasCookie

    , accept
    , contentType

    , fromVault

    , module Network.Wai.Predicate.MediaType
    , module Network.Wai.Predicate.Error
    ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.CaseInsensitive (original)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.Predicate
import Data.Vault.Lazy (Key)
import Data.Word
import Network.HTTP.Types
import Network.Wai.Predicate.Accept
import Network.Wai.Predicate.Content
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility
import Network.Wai

import qualified Data.Vault.Lazy as Vault

def :: a -> Predicate r Error a -> Predicate r Error a
def a = fmap $
    result (\e -> if not (TypeError `isReasonOf` e) then return a else Fail e)
           Okay

opt :: Predicate r Error a -> Predicate r Error (Maybe a)
opt = fmap $
    result (\e -> if not (TypeError `isReasonOf` e) then return Nothing else Fail e)
           (\d -> Okay d . Just)

request :: HasRequest r => Predicate r f Request
request = return . getRequest

query :: (HasQuery r, FromByteString a) => ByteString -> Predicate r Error a
query k r =
    case lookupQuery k r of
        [] -> Fail . addLabel "query" $ notAvailable k
        qq -> either (Fail . addLabel "query" . typeError k)
                     return
                     (readValues qq)

hasQuery :: HasQuery r => ByteString -> Predicate r Error ()
hasQuery k r =
    when (null (lookupQuery k r)) $
        (Fail . addLabel "query" $ notAvailable k)

header :: (HasHeaders r, FromByteString a) => HeaderName -> Predicate r Error a
header k r =
    case lookupHeader k r of
        [] -> Fail . addLabel "header" $ notAvailable (original k)
        hh -> either (Fail . addLabel "header" . typeError (original k))
                     return
                     (readValues hh)

hasHeader :: HasHeaders r => HeaderName -> Predicate r Error ()
hasHeader k r =
    when (isNothing (find ((k ==) . fst) (headers r))) $
        (Fail . addLabel "header" $ notAvailable (original k))

segment :: (HasPath r, FromByteString a) => Word -> Predicate r Error a
segment i r =
    case lookupSegment i r of
        Nothing -> Fail $
            e400 & setMessage "Path segment index out of bounds."
                 . addLabel "path"
        Just  s -> either (\m -> Fail (e400 & addLabel "path" . setReason TypeError . setMessage m))
                          return
                          (readValues [s])

hasSegment :: HasPath r => Word -> Predicate r Error ()
hasSegment i r =
    when (isNothing (lookupSegment i r)) $
        Fail (e400 & addLabel "path" . setMessage "Path segment index out of bounds.")

cookie :: (HasCookies r, FromByteString a) => ByteString -> Predicate r Error a
cookie k r =
    case lookupCookie k r of
        [] -> Fail . addLabel "cookie" $ notAvailable k
        cc -> either (Fail . addLabel "cookie" . typeError k)
                     return
                     (readValues cc)

hasCookie :: HasCookies r => ByteString -> Predicate r Error ()
hasCookie k r =
    when (null (lookupCookie k r)) $
        (Fail . addLabel "cookie" $ notAvailable k)

fromVault :: HasVault r => Key a -> Predicate r Error a
fromVault k r =
    case Vault.lookup k (requestVault r) of
        Just  a -> return a
        Nothing -> Fail $
            e500 & setReason NotAvailable
                 . setMessage "Vault does not contain key."
                 . addLabel "vault"

-----------------------------------------------------------------------------
-- Internal

notAvailable :: ByteString -> Error
notAvailable k = e400 & setReason NotAvailable . setSource k
{-# INLINE notAvailable #-}

typeError :: ByteString -> ByteString -> Error
typeError k m = e400 & setReason TypeError . setSource k . setMessage m
{-# INLINE typeError #-}

