-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate
    ( module Data.Predicate
    , getRequest

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
    , module Network.Wai.Predicate.MediaType
    , module Network.Wai.Predicate.Error
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.CaseInsensitive (mk)
import Data.List (find)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Predicate
import Data.Word
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Accept
import Network.Wai.Predicate.Content
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility
import Network.Wai

getRequest :: (HasRequest r) => Predicate r f Request
getRequest = return . request

query :: (HasQuery r, FromByteString a) => ByteString -> Predicate r Error a
query k r = case lookupQuery k r of
    [] -> Fail (err status400 ("Missing query '" <> k <> "'."))
    qq -> either (Fail . err status400) return (readValues qq)

hasQuery :: (HasQuery r) => ByteString -> Predicate r Error ()
hasQuery k r =
    if null (lookupQuery k r)
        then Fail (err status400 ("Missing query '" <> k <> "'."))
        else return ()

header :: (HasHeaders r, FromByteString a) => ByteString -> Predicate r Error a
header k r = case lookupHeader k r of
    [] -> Fail (err status400 ("Missing header '" <> k <> "'."))
    hh -> either (Fail . err status400) return (readValues hh)

hasHeader :: (HasHeaders r) => ByteString -> Predicate r Error ()
hasHeader k r =
    if isJust $ find ((mk k ==) . fst) (headers r)
        then return ()
        else Fail (err status400 ("Missing header '" <> k <> "'."))

segment :: (HasPath r, FromByteString a) => Word -> Predicate r Error a
segment i r = case lookupSegment i r of
    Nothing -> Fail (err status400 "Path segment index out of bounds.")
    Just  s -> either (Fail . err status400) return (readValues [s])

hasSegment :: (HasPath r) => Word -> Predicate r Error ()
hasSegment i r =
    if isJust $ lookupSegment i r
        then return ()
        else Fail (err status400 "Path segment index out of bounds.")

cookie :: (HasCookies r, FromByteString a) => ByteString -> Predicate r Error a
cookie k r = case lookupCookie k r of
    [] -> Fail (err status400 ("Missing cookie '" <> k <> "'."))
    cc -> either (Fail . err status400) return (readValues cc)

hasCookie :: (HasCookies r) => ByteString -> Predicate r Error ()
hasCookie k r =
    if null (lookupCookie k r)
        then Fail (err status400 ("Missing cookie '" <> k <> "'."))
        else return ()
