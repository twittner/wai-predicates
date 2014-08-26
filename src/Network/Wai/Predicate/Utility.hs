-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Utility (readValues, (&)) where

import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.List (foldl')
import Data.String (fromString)

readValues :: FromByteString a => [ByteString] -> Either ByteString a
readValues = foldl' res (Left "no parse") . map (runParser parser)
  where
    res (Left  _) (Right x) = Right x
    res (Right x) _         = Right x
    res _         (Left  x) = Left (fromString x)

infixl 1 &
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
