-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Predicate.Error
    ( Error
    , Reason (..)
    , err
    , e400
    , e405
    , e406
    , e415
    , e500
    , status
    , message
    , reason
    , source
    , labels
    , setStatus
    , setMessage
    , setReason
    , setSource
    , addLabel
    , isReasonOf
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Status

data Reason
    = NotAvailable
    | TypeError
    deriving (Eq, Show)

-- | The error type used as meta-data for @Fail@ in all WAI predicates.
data Error = Error
    { status  :: !Status          -- ^ HTTP status code
    , message :: Maybe ByteString -- ^ optional status message
    , reason  :: Maybe Reason     -- ^ optional reason for this error
    , source  :: Maybe ByteString -- ^ optional source of this erro
    , labels  :: [ByteString]     -- ^ optional free-text labels
    } deriving (Eq, Show)

err :: Status -> Error
err s = Error s Nothing Nothing Nothing []

setStatus :: Status -> Error -> Error
setStatus s e = e { status = s }

setMessage :: ByteString -> Error -> Error
setMessage m e = e { message = Just m }

setReason :: Reason -> Error -> Error
setReason r e = e { reason = Just r }

setSource :: ByteString -> Error -> Error
setSource s e = e { source = Just s }

addLabel :: ByteString -> Error -> Error
addLabel l e = e { labels = l : labels e }

isReasonOf :: Reason -> Error -> Bool
isReasonOf r e = maybe False (r ==) (reason e)

e400, e405, e406, e415, e500 :: Error
e400 = err status400
e405 = err status405
e406 = err status406
e415 = err status415
e500 = err status500

