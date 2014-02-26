{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tests.Wai.Predicate (tests) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Status
import Network.Wai.Routing
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Wai.Util

tests :: TestTree
tests = testGroup "Wai.Predicate"
    [ testCase "Accept application/json" testAcceptJson
    , testCase "Accept application/thrift " testAcceptThrift
    , testCase "Accept application/*" testAcceptAll
    , testCase "Content-Type text/plain" testContentTypePlain
    , testCase "Content-Type text/*" testContentTypeAll
    , testCase "Query" testQuery
    , testCase "QueryOpt" testQueryOpt
    ]

testAcceptJson :: IO ()
testAcceptJson = do
    let rq0 = fromWaiRequest [] . json $ request "/"
    T 0 (Media "application" "json" 1.0 []) @=? apply (accept :: Accept "application" "json") rq0

    let rq1 = fromWaiRequest [] . withHeader "Accept" "foo/bar" $ request "/"
    F (err status406 ("Expected 'Accept: application/json'.")) @=? apply (accept :: Accept "application" "json") rq1

testAcceptThrift :: IO ()
testAcceptThrift = do
    let rq0 = fromWaiRequest [] . withHeader "Accept" "application/x-thrift" $ request "/"
    T 0 (Media "application" "x-thrift" 1.0 []) @=? apply (accept :: Accept "application" "x-thrift") rq0

    let rq1 = fromWaiRequest [] . json $ request "/"
    F (err status406 ("Expected 'Accept: application/x-thrift'.")) @=? apply (accept :: Accept "application" "x-thrift") rq1

testAcceptAll :: IO ()
testAcceptAll = do
    let rq0 = fromWaiRequest [] . withHeader "Accept" "application/*" $ request "/"
    T 0 (Media "application" "*"    1.0 []) @=? apply (accept :: Accept "application" "*") rq0
    T 0 (Media "application" "json" 1.0 []) @=? apply (accept :: Accept "application" "json") rq0

testContentTypePlain :: IO ()
testContentTypePlain = do
    let rq0 = fromWaiRequest [] . withHeader "Content-Type" "text/plain" $ request "/"
    T 0 (Media "text" "plain" 1.0 []) @=? apply (contentType :: ContentType "text" "plain") rq0

    let rq1 = fromWaiRequest [] . withHeader "Content-Type" "text/html" $ request "/"
    F (err status415 ("Expected 'Content-Type: text/plain'.")) @=? apply (contentType :: ContentType "text" "plain") rq1

testContentTypeAll :: IO ()
testContentTypeAll = do
    let rq0 = fromWaiRequest [] . withHeader "Content-Type" "text/plain" $ request "/"
    T 0.5 (Media "text" "plain" 0.5 []) @=? apply (contentType :: ContentType "text" "*") rq0

testQuery :: IO ()
testQuery = do
    let rq0 = fromWaiRequest [] . withQuery "x" "y" . withQuery "x" "z" $ request "/"
    T 0 "y" @=? apply (query "x" :: Query ByteString) rq0

    let rq1 = fromWaiRequest [] $ request "/"
    F (err status400 ("Missing query 'x'.")) @=? apply (query "x" :: Query ByteString) rq1

testQueryOpt :: IO ()
testQueryOpt = do
    let rq0 = fromWaiRequest [] . withQuery "x" "y" . withQuery "x" "z" $ request "/"
    T 0 (Just "y") @=? apply (opt (query "x" :: Query ByteString)) rq0

    let rq1 = fromWaiRequest [] $ request "/"
    T 0 Nothing @=? apply (opt (query "x" :: Query ByteString)) rq1

